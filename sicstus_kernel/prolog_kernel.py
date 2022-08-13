"""
Base class for a Prolog Jupyter kernel communicating with a Prolog server with JSON-RPC 2.0 messages.
The JSON-RPC 2.0 communication is based on 'jsonrpc_client.py' from SICStus 4.5.1
"""


from ipykernel.kernelbase import Kernel
import os
import sys
import subprocess
import json
import logging
import errno
from signal import signal, SIGINT
from IPython.utils.tokenutil import line_at_cursor
from IPython.core.completer import CompletionSplitter
import graphviz


# Enable logging
logging.basicConfig(level=logging.DEBUG,
                    format='[%(asctime)s] {%(filename)s:%(lineno)d} %(levelname)s - %(message)s')

output_text_style = """
font-family: Menlo, Consolas, 'DejaVu Sans Mono', monospace;
font-size: 13px;
line-height: 1.3077;
"""


class PrologBaseKernel(Kernel):
    language_info = {
        'name': 'Prolog',
        'file_extension': '.pl',
        'mimetype': 'text/x-prolog',
        'codemirror_mode': 'prolog',
    }

    logger = logging.getLogger()

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # Run handle_signal_interrupt when the kernel is interrupted
        signal(SIGINT, self.handle_signal_interrupt)

        # Start the Prolog server
        self.prolog_proc = None
        self.start_prolog_server()
        self.is_server_restart_required = False

        self.configure_token_splitters()
        self.retrieve_predicate_information()


    def handle_signal_interrupt(self, signal_received, frame):
        self.handle_interrupt()


    def handle_interrupt(self):
        # Interrupting the kernel interrupts the Prolog process, so it needs to be restarted
        self.logger.debug('Kernel interrupted')
        self.kill_prolog_server()


    def start_prolog_server(self):
        """
        Tries to (re)start the prolog server process.
        """
        self.kill_prolog_server()
        self.prolog_proc = None
        # Start the Prolog server
        if os.path.exists(self.pl_path):
            self.prolog_proc = subprocess.Popen(
                self.program_arguments,
                stdout=subprocess.PIPE,
                stdin=subprocess.PIPE,
                stderr=subprocess.PIPE,
                encoding='UTF-8'
            )
            self.is_server_restart_required = False
        else:
            self.logger.debug('The file ' + self.pl_path + ' could not be found in ' + os.getcwd(), exc_info=True)


    def kill_prolog_server(self):
        """
        Kills the prolog server process if it is still running.
        """
        if self.prolog_proc is not None:
            self.logger.debug('Kill Prolog server')
            self.prolog_proc.kill()
            self.is_server_restart_required = True


    def configure_token_splitters(self):
        """Configure splitters which are used to determine the token which is to be completed/inspected"""
        splitter_delims = ' ,\t\n()[{]}|;\'"'
        self.completion_splitter = CompletionSplitter()
        self.completion_splitter.delims = splitter_delims
        # For the inspection additionally use ':' as a delimiter for splitting as most of the predicate names the tokens are compared to are not module name expanded
        self.inspection_splitter = CompletionSplitter()
        self.inspection_splitter.delims = splitter_delims + ':'


    def retrieve_predicate_information(self):
        """Request information from the Prolog server which is needed for code completion and inspection"""
        try:
            # The currently defined predicates are used for code completion
            predicates_response_dict = self.server_request(0, 'predicates')
            self.current_predicates = predicates_response_dict["result"]

            # Retrieve the documentation texts which are shown when a predicate provided by the Prolog server in the module 'Jupyter' is inspected
            jupyter_predicate_docs_dict = self.server_request(0, 'jupyter_predicate_docs')
            self.jupyter_predicate_docs = jupyter_predicate_docs_dict["result"]

        except Exception as exception:
            self.logger.error(exception, exc_info=True)


    # Overriden kernel methods


    def do_execute(self, code, silent, store_history=True, user_expressions=None, allow_stdin=False):
        """
        A request to execute code was received.
        The code is tried to be executed by sending it to the Prolog server.

        If the execution is interrupted or an exception occurs, an error response is sent to the frontend.
        """
        self.logger.debug('code: ' + code)

        if not silent:
            try:
                # Check if the server had been shutdown (because of 'halt', an interrupt, or an exception) and a server restart is necessary
                if self.is_server_restart_required:
                    self.logger.debug('Restart Prolog server')
                    self.start_prolog_server()
                    self.send_response_display_data(self.informational_prefix + 'The Prolog server was restarted', "color:red")

                # Send an execution request and handle the response
                response_dict = self.server_request(self.execution_count, 'call', {'code':code})
                if 'result' in response_dict:
                    reply_object = self.handle_success_response(response_dict)
                else:
                    # 'error' in response_dict:
                    reply_object = self.handle_error_response(response_dict)

            except KeyboardInterrupt:
                self.handle_interrupt()
                return {'status': 'error', 'ename' : 'interrupt', 'evalue' : '', 'traceback' : ''}
            except BrokenPipeError:
                self.logger.error(self.error_prefix + 'Broken pipe\n' + self.error_prefix + 'The Prolog server needs to be restarted', "color:red")
                self.is_server_restart_required = True
                self.send_response_display_data(self.error_prefix + 'Something went wrong\n' + self.error_prefix + 'The Prolog server needs to be restarted\n', "color:red")
                return {'status': 'error', 'ename' : 'broken pipe', 'evalue' : '', 'traceback' : ''}
            except Exception as exception:
                self.logger.error(exception, exc_info=True)
                self.is_server_restart_required = True
                self.send_response_display_data(self.error_prefix + 'Something went wrong\n' + self.error_prefix + 'The Prolog server needs to be restarted\n', "color:red")
                return {'status': 'error', 'ename' : 'exception', 'evalue' : '', 'traceback' : ''}

        return reply_object


    def server_request(self, id, method, params=None):
        """
        Sends a request to the Prolog server, reads the JSON response, deserializes and returns it.

        If something goes wrong, raises an exception so that an error response is sent to the frontend.

        Raises
        ------
        FileNotFoundError if the Prolog process does not exist
        JSONDecodeError if the response could not be deserialized
        """
        if self.prolog_proc is None:
            # The Prolog server could not be started because the code file could not be found
            # TODO adjust when uploading to PyPi
            self.send_response_display_data('The file ' + self.pl_path + ' could not be found in ' + os.getcwd(), "color:red")
            raise FileNotFoundError(errno.ENOENT, os.strerror(errno.ENOENT), self.pl_path)

        # Create a JSON-RCP Request object (https://www.jsonrpc.org/specification#request_object)
        if params is None:
            request = json.dumps({'jsonrpc':'2.0', 'id':id, 'method':method})
        else:
            request = json.dumps({'jsonrpc':'2.0', 'id':id, 'method':method, 'params':params})

        # Send the request to the Prolog server
        self.prolog_proc.stdin.write(request)
        self.prolog_proc.stdin.flush()

        # Read the JSON-RCP Response object (http://www.jsonrpc.org/specification#response_object)
        response_string = self.prolog_proc.stdout.readline()
        self.logger.debug('response: ' + response_string)

        try:
            return json.loads(response_string)
        except json.decoder.JSONDecodeError as exception:
            self.logger.debug('The Response object is no valid JSON object')
            raise


    def handle_success_response(self, response_dict):
        """
        The dictionary response_dict contains the key 'result'.
        The corresponding value is a dictionary containing the results of the Prolog terms read from the cell.
        These are given as a dictionary where the keys are integers starting from 1.

        Each of the results has a status which is either 'halt', 'success', or 'error'.
        For each result, a corresponding response is sent to the client.

        In case of CLPFD a variable might not have been assigned a single value, but a domain instead.
        In that case, the dictionary value of the variable is a dictionary where the value of 'dom' is a string representing the domain.
        If there is a single value, the binding is displayed by the frontend with a '=' (e.g. M = 1).
        Otherwise, it is displayed with 'in' (e.g. X in 1..3).

        Example
        ------
        For the cell code
          "member(M, [1,2,3]), print(M)."
        the result dictionary is:
        {
          "1": {
            "status": "success",
            "type": "query",
            "bindings": {
              "M": "1"
            },
            "output": ""
          }
        }
        The following variable bindings are sent to the frontend:
          M = 1

        CLPFD Example
        ------
        For the cell code
          "X in 1..7, Y #= X+X, Y #\= 3."
        the result dictionary is:
        {
          "1": {
            "status": "success",
            "type": "query",
            "bindings": {
              "X": {
                "dom": "1..7"
              },
              "Y": {
                "dom": "{2}\\/(4..14)"
              }
            },
            "output": ""
          }
        }
        The following variable bindings are sent to the frontend:
          X in 1..7,
          Y in {2}\/(4..14)
        """

        # Read the term results
        result = response_dict["result"]

        if result is None or result == '':
            return {
                'status': 'ok',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
            }

        is_error = False

        index = 1
        while str(index) in result:
            term_result = result[str(index)]
            status = term_result["status"]

            if status == "halt":
                # The Prolog server was stopped, so it has to be restarted the next time code is to be executed
                self.kill_prolog_server()
                self.send_response_display_data(self.informational_prefix + 'Successfully halted')
            elif status == "error":
                is_error = True
                self.handle_error_response(term_result)
            elif status == "success":
                self.handle_additional_data(term_result)

                # Send the output to the frontend
                output = term_result["output"]
                if output != "":
                    self.send_response_display_data(str(output))

                type = term_result["type"]
                if type == "query":
                    # Send the variable names and values or "yes" to the frontend
                    bindings = term_result["bindings"]
                    if bindings == {}:
                        response_text = self.success_response
                    else:
                        # Read the variable values
                        variable_values = []
                        for var, val in bindings.items():
                            if isinstance(val, dict):
                                if 'dom' in val:
                                    # CLPFD: the variable has not been assigned a single value, but a domain
                                    variable_values.append(str(var) + ' in ' + str(val['dom']))
                            else:
                                variable_values.append(str(var) + ' = ' + str(val))
                        response_text = ",\n".join(variable_values)

                    self.send_response_display_data(response_text, 'font-weight: bold;')
            index = index + 1

        # If at least one of the terms caused an error, an error reply is sent to the client
        if is_error:
            return {
               'status' : 'error',
               'ename' : 'error',
               'evalue' : '',
               'traceback' : [],
            }
        else:
            return {
                'status': 'ok',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
            }


    def handle_error_response(self, response_dict):
        """
        The dictionary response_dict contains the key 'error'.
        The corresponding value is a dictionary containing the error data.
        The member 'data' can contain members 'error_info (e.g. a more specific error message) and 'output' (output of the request before the error occurred).

        Example
        ------
        For the cell code
          ":- append([1], [2], Res), print(Res).
          :- append(1, 2, Res), print(Res)."
        the error dictionary is:
        {
            "code": -4712,
            "message": "Exception",
            "data": {
                "error_info": "* append(1,2,Res),print(Res) - goal failed",
                "output": "[1,2]"
            }
        }
        """

        error = response_dict["error"]
        error_code = error['code']
        self.logger.debug('error code: ' + str(error_code))

        if error['data']:
            self.handle_additional_data(error['data'])

            # Send the output to the client
            if 'output' in error['data']:
                output = error['data']['output']
                self.send_response_display_data(output)
            else:
                output = ''

        if error_code == -4711:
            ename = 'failure'
            response_text = self.failure_response
        elif error_code == -4712:
            # Exception: "error_info" contains the error message
            ename = 'exception'
            output += '\n' + error['data']['error_info']
            response_text = error['data']['error_info'] + '\n'
        elif error_code == -4715:
            # Unhandled exception: the server needs to be restarted
            ename = 'unhandled exception'
            output += '\n' + error['data']['error_info']
            self.kill_prolog_server()
            response_text = error['data']['error_info'] + '\n' + self.error_prefix + 'The Prolog server needs to be restarted'
        else:
            ename = 'error'
            output += '\n' + error['message']
            response_text = self.error_prefix + str(error['message']) + '\n'

        self.send_response_display_data(response_text, "color:red; font-weight:bold;")

        return {
           'status' : 'error',
           'ename' : ename,
           'evalue' : '',
           'traceback' : [output],
        }


    def handle_additional_data(self, dict):
        """
        Handles additional data which may be present in the dict
        """
        if 'retracted_clauses' in dict:
            self.handle_retracted_clauses(dict['retracted_clauses'])
        if 'print_table' in dict:
            self.handle_print_table(dict['print_table'])
        if 'print_sld_tree' in dict:
            self.handle_print_graph(dict['print_sld_tree'])
        if 'print_transition_graph' in dict:
            self.handle_print_graph(dict['print_transition_graph'])


    def handle_retracted_clauses(self, retracted_clauses):
        """
        Handles the displaying of clauses which were retracted.
        Since this might not always be of interest for the user, a HTML <details> tag is used with which information can be displayed which can be expanded.
        The dictionary retracted_clauses contains the retracted clauses.
        The keys are the predicate specs and the values are the actual clauses which were retracted as output by listing/1.

        Example
        ------
        {'user:app/3': 'app([], A, A) :- !.\napp([A|B], C, [A|D]) :-\n        app(B, C, D).\n',
         'user:app/4': 'app(A, B, C, D) :-\n        app(B, C, E),\n        app(A, E, D).\n'}
        """
        if retracted_clauses:
            style = """
            <style>
            details  {
            """ + output_text_style + """
            }

            details > summary {
              cursor: pointer;
            }
            </style>
            """

            # For each entry in retracted_clauses, create a details object
            html_details = ''
            plain_text = ''
            for pred_spec, listing in retracted_clauses.items():
                html_details += '<details open><summary>' + pred_spec + '</summary><pre>' + listing + '</pre><br></details>'
                plain_text += pred_spec + ':\n' + listing + '\n'

            # Create an expandable message saying that clauses have been retracted
            html = '<details><summary>Some previously defined clauses were retracted (click to expand)</summary>' + html_details + '</details>'
            plain_text = 'Some previously defined clauses were retracted:\n' + plain_text

            display_data = {
                'data': {
                    'text/plain': plain_text,
                    'text/html': style + html },
                'metadata': {
                    'application/json' : {}}}
            self.send_response(self.iopub_socket, 'display_data', display_data)


    def handle_print_table(self, print_table_dict):
        """
        The dictionary print_table_dict contains the members 'ValuesLists' and 'VariableNames'.
        The values are used to populate a table which is sent to the client.

        Example
        ------
        For the cell code
          "print_table((member(Member, [10,20,30]), Square is Member*Member))."
        the variables dictionary is:
          {'ValuesLists': [['10', '100'], ['20', '400'], ['30', '900']], 'VariableNames': ['Member', 'Square']}
        The markdown text send to the frontend is:
          Member | Square |
          :- | :- |
          10 | 100 |
          20 | 400 |
          30 | 900 |
        """
        values_lists = print_table_dict["ValuesLists"]
        variable_names = print_table_dict["VariableNames"]

        table_header_markdown_string = ""
        table_markdown_string = ""

        # Create a header line with the variable names
        for variable_name in variable_names:
            table_header_markdown_string = table_header_markdown_string + str(variable_name) + " | "
            table_markdown_string = table_markdown_string + ":- | "

        table_markdown_string = table_header_markdown_string  + "\n" + table_markdown_string

        # For each values list, add a markdown table line
        for values_list in values_lists:
            line_markdown_string = ""
            for value in values_list:
                line_markdown_string = line_markdown_string + str(value) + " | "

            table_markdown_string = table_markdown_string + "\n" + line_markdown_string

        display_data = {'data': {'text/plain': table_markdown_string, 'text/markdown': table_markdown_string.replace('$', '\$')}, 'metadata': {}}
        self.send_response(self.iopub_socket, 'display_data', display_data)


    def handle_print_graph(self, graph_file_content):
        """
        The string graph_file_content corresponds to the content of a file from which a svg file can be rendered with dot.
        This file is rendered and sent to the client.

        Example
        ------
        graph_file_content can look like the following
        graph {
            "1" [label="user:pred"]
            "2" [label="user:sub_goal_1"]
            "3" [label="user:sub_goal_1_1"]
            "4" [label="user:sub_goal_1_2"]
            "5" [label="user:sub_goal_2"]
            "6" [label="user:sub_goal_2_1"]
            "7" [label="user:sub_goal_2_2"]
            "1" -- "2"
            "2" -- "3"
            "2" -- "4"
            "1" -- "5"
            "5" -- "6"
            "5" -- "7"
        }
        """

        # Write the content to a file
        f = open("graph.gv", "w")
        f.write(graph_file_content)
        f.close()

        # Render a svg file
        graphviz.render(engine='dot', format='svg', filepath='graph.gv', outfile='graph.svg').replace('\\', '/')

        # Read the svg file content
        svg_file = open("graph.svg", "r")
        svg_content = svg_file.read()

        # Remove the created files
        os.remove("graph.gv")
        os.remove("graph.svg")

        # Send the data to the client
        display_data = {
            'data': {
                'text/plain': graph_file_content,
                'image/svg+xml': svg_content,
                #'text/latex': "", TODO?
            },
            'metadata': {}}
        self.send_response(self.iopub_socket, 'display_data', display_data)


    def send_response_display_data(self, text, additional_style=""):
        # The display data of the response always needs to contain plain text
        # For example, this is shown in Jupyter Console
        # Additionally, markdown text is provided for a formatted output in a Jupyter notebook
        # In order to be able to export notebooks as LaTeX, latex text needs to be returned
        display_data = {
            'data': {
                'text/plain': text,
                'text/markdown': '<pre style="' + output_text_style + additional_style + '">' + text + '</pre>',
                'text/latex': text.replace('\n', '\\\\\n ') # TODO adjust for latex
            },
            'metadata': {}}
        self.send_response(self.iopub_socket, 'display_data', display_data)


    def do_shutdown(self, restart):
        self.kill_prolog_server()
        return {'status': 'ok', 'restart': restart}


    def do_complete(self, code, cursor_pos):
        if self.current_predicates is None:
            return {'matches' : [],
                    'cursor_end' : cursor_pos,
                    'cursor_start' : cursor_pos,
                    'metadata' : {},
                    'status' : 'ok'}

        token = self.get_current_token(self.completion_splitter, code, cursor_pos)

        # Find the matching predicates
        # If a key of the dictionary contains the current token, the element is assumed to match
        matching_predicates = [pred for pred in self.current_predicates if (token in pred)]

        return {'matches' : matching_predicates,
                'cursor_end' : cursor_pos,
                'cursor_start' : cursor_pos - len(token),
                'metadata' : {},
                'status' : 'ok'}


    def do_inspect(self, code, cursor_pos, detail_level=0, omit_sections=()):
        """
        Inspection is supported for the predicates from module jupyter.
        By overriding this method, inspection for further predicates can be implemented.
        """
        token, data = self.get_token_and_jupyter_predicate_inspection_data(code, cursor_pos)

        if data == {}:
            found = False
        else:
            found = True

        return {'status': 'ok', 'data': data, 'metadata': {}, 'found': found}


    def get_token_and_jupyter_predicate_inspection_data(self, code, cursor_pos):
        token = self.get_current_token(self.inspection_splitter, code, cursor_pos)

        if not token:
            # There is no token which can be inspected
            data = {}
        else:
            # Find all matching predicate inspection data
            # If a key of the dictionary contains the current token, the element is assumed to match
            matching_predicate_data = {pred:self.jupyter_predicate_docs[pred] for pred in self.jupyter_predicate_docs if (token in pred)}

            if len(matching_predicate_data) == 0:
                # There is no matching predicate
                data = {}
            else:
                 # Compute plain text and markdown output for the matching predicate data
                jupyter_docs_plain = ''
                jupyter_docs_md = ''
                for pred, data in matching_predicate_data.items():
                    jupyter_docs_plain += data + '\n\n'
                    jupyter_docs_md += '<pre>' + data.replace('\n', '<br>').replace('$', '&#36;') + '<br><br></pre>'

                data = {'text/plain': jupyter_docs_plain, 'text/markdown': jupyter_docs_md}

        return token, data


    def get_current_token(self, splitter, code, cursor_pos):
        if cursor_pos is None:
            cursor_pos = len(code)
        # Get the line where the cursor is and the character offset of the start of the line
        line, offset = line_at_cursor(code, cursor_pos)
        line_cursor = cursor_pos - offset

        # Get the current token in the line
        return splitter.split_line(line, line_cursor)
