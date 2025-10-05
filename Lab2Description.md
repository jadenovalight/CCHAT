# Lab 2: CCHAT - TDA384 / DIT392 Principles of Concurrent Programming

## Deadlines
See labs page

## Demo Registrations
- Register for one time slot in one of the Links - Polls (will be available a few days before the demo).
- Before accessing the poll, supply a name using your group number in Fire.
- Try to split evenly among the available rooms.
- For the demo:
  - Join the respective Links - Zoom meeting at least 10 minutes before your demo starts.
  - Have everything ready to run.
  - Change your Zoom name to your group number.
- All demos are online over Zoom.


## Lab Description
In this lab, you will build a simple text-based messaging system called **CCHAT**, heavily inspired by IRC (a standard for group discussions). For simplicity:
- CCHAT does not use IRC’s protocol or low-level TCP/IP communication.
- It leverages Erlang’s processes and message passing features.

### Programming Language
This lab assignment must be developed in **Erlang**.

### How to Install Erlang/OTP
Refer to standard Erlang/OTP installation guides for your operating system.

### Overview
A video demonstrating the expected functionality of CCHAT is available at:  
https://chalmers.instructure.com/courses/35810/pages/lab-2-cchat


## CCHAT Usage & Interfaces
### 1. GUI-based Erlang Functions
- `cchat:server()`: Starts a chat server process and registers it to a pre-defined name. All users chat through this server.
- `cchat:client()`: Opens a new client window for entering chat commands (listed below).

### 2. Terminal-based Erlang Functions
A terminal-based interface is provided for cases where the GUI fails (e.g., lab computers). Instructions for starting terminal-based clients are at the end of this document.

### 3. Chat Commands
| Command               | Description                                                                 |
|-----------------------|-----------------------------------------------------------------------------|
| `/join #channel`      | Join a channel (channel names must start with `#` by convention).           |
| `/leave`              | Leave the current channel.                                                 |
| `/leave #channel`     | Leave the specified channel.                                                |
| `/whoami`             | Return the current nick (displayed in the System tab).                      |
| `/nick newnick`       | Change nick to `newnick` (format: starts with lowercase, followed by letters/numbers/`_`). |
| `/quit`               | Quit the client.                                                           |


## Architecture
CCHAT uses a **client/server architecture**, consisting of:
- **Client part**: Runs the GUI and a client process.
- **Server part**: Hosts chat channels.

### Location of Processes
- For simplicity, all processes run on the same local machine.
- A correct implementation can be easily adapted to distributed environments (see "Location transparency & distribution" in Lecture 7).

### Key Components
#### 1. GUI and Client Process
- The GUI is **pre-provided** (no need to implement it).
- The client process (your implementation) must interact with the GUI via a fixed protocol (detailed below).
- If the protocol is not followed, code will fail tests and be rejected.

#### 2. Server Process
- Handles requests from clients.
- The client-server communication protocol is defined by you.
- The server may consist of multiple processes (introduce concurrency where appropriate).


## The GUI/Client Protocol
The protocol between the GUI and client process is fixed. It defines:
- How the GUI requests operations.
- How the client responds to requests.
- Asynchronous communication from client to GUI.

### 1. Request/Response Flow
- **Success**: Client replies with the atom `ok`.
- **Recoverable Error**: Client replies with the tuple `{error, Atom, Text}`, where:
  - `Atom`: A fixed error identifier (defined in the "Error Handling" section).
  - `Text`: Custom error message (displayed in the GUI’s System tab).
- **Fatal Error**: Client replies with `{'EXIT', Reason}` (indicates critical failure, e.g., server crash). The GUI displays `Reason` and exits (should not appear in final submissions).

### 2. Supported Operations (GUI → Client)
#### Joining a Channel
- GUI sends: `{join, Channel}` (when user enters `/join #channel`).
- Server behavior: Creates the channel if it does not exist (channels persist for the server’s lifetime).
- Error: Return `user_already_joined` if the user tries to join a channel they are already in.
- Note: Only joined users can send messages to a channel.

#### Writing Messages to a Channel
- GUI sends: `{message_send, Channel, Msg}` (when user enters text not starting with `/`).
- Error: Return `user_not_joined` if the user tries to message a channel they have not joined.

#### Leaving a Channel
- GUI sends: `{leave, Channel}` (when user enters `/leave` or `/leave #channel`).
- Error: Return `user_not_joined` if the user tries to leave a channel they have not joined.

#### Asking for the Nickname
- GUI sends: `whoami` (when user enters `/whoami`).
- Client response: Return the current nick as a **string** (not `ok`). No errors for this operation.

#### Changing the Nickname
- GUI sends: `{nick, Name}` (when user enters `/nick newnick`).
- **Distinction Assignment (2 points)**: Return `nick_taken` if the new nick is already in use.

### 3. Asynchronous Communication (Client → GUI)
- The only client-initiated communication: When a message needs to be displayed in a channel.
- Client sends: `{message_receive, Channel, String}`, where `String` is the text to show in `Channel`.


## Error Handling
### 1. Recoverable Errors
Return `{error, Atom, Text}` for non-fatal issues. Use the following fixed atoms:
| Atom               | Scenario                                                                 |
|--------------------|--------------------------------------------------------------------------|
| `user_already_joined` | User tries to join a channel they are already in.                       |
| `user_not_joined`     | User tries to message/leave a channel they have not joined.             |
| `nick_taken`          | User tries to change to an already used nick (distinction assignment).  |
| `server_not_reached`  | Server is non-existent or unresponsive.                                 |

### 2. Fatal Errors
Return `{'EXIT', Reason}` for critical failures (e.g., server crash mid-request). This should not appear in tested submissions.


## Assignment Requirements
### 1. Implementation Tasks
- Implement all `TODO` sections in `client.erl` and `server.erl`.
- Optional: Create extra modules for code organization.
- Ensure code passes all test cases.
- **Mandatory**: Use message passing for client/server communication. Direct function calls between modules are strictly forbidden.

### 2. Submission Guidelines
- Submit the following files:
  - `client.erl`
  - `server.erl`
  - Any other files required for your solution to run.
- **Do NOT submit**:
  - The `lib` folder (contains pre-provided files like GUI and testing code).
  - Compressed archives (e.g., `.zip`, `.tar.gz`). Upload individual source files.
- Add comments to your code for grader readability. No separate documentation is required.


## Code Skeleton
### Download
The skeleton code package is available via the course link (contains pre-provided and editable files).

### File Structure
| File/Directory       | Description                                                                 | Editable? |
|----------------------|-----------------------------------------------------------------------------|-----------|
| `client.erl`         | Client process (implements `client:handle/2` for GUI requests).             | Yes       |
| `server.erl`         | Server process (implement `start/1`, `stop/1`, and server logic).          | Yes       |
| `lib/`               | Internal libraries (pre-provided).                                          | No        |
| `lib/cchat.erl`      | Top-level module (launches server/clients with GUI).                        | No        |
| `lib/gui.erl`        | GUI implementation (and related files: `lexgrm.erl`, `grm.yrl`, etc.).     | No        |
| `lib/genserver.erl`  | Custom generic server module (for spawning processes and sync messaging).  | No        |
| `lib/test_client.erl`| Unit tests (for validating functionality).                                 | No        |
| `lib/dummy_gui.erl`  | Dummy GUI for testing.                                                     | No        |


## Compilation & Testing
### 1. Compilation
Compile all files with:
```bash
make all
```
After compilation:
- Start the Erlang shell with `erl`.
- Launch a client with `cchat:client()` (GUI will open, but communication will not work until core logic is implemented).

### 2. Testing
- Run all unit tests (in `lib/test_client.erl`) with:
  ```bash
  make -s run_tests
  ```
- Notes:
  - Failing tests indicate incorrect functionality.
  - Passing all tests does not guarantee submission acceptance (additional manual checks may apply).
  - Disable color codes in test output (if needed):
    - Comment out the `colour` function in `lib/test_client.erl`.
    - Replace it with: `colour(Num,S) -> S.`


## Tips for Implementation
1. **Getting Started**:
   - First implement `server:start/1` (spawn a server process that replies with a default message).
   - Establish client-server communication (e.g., implement the `/join` command).
   - Fix failing tests one by one.

2. **Server Registration**:
   - The server process must be registered to the atom provided to `server:start/1` (e.g., if the server name is `shire`, send messages directly to the `shire` atom).

3. **Concurrency**:
   - Introduce concurrency where suitable (e.g., per-channel processes). Some tests fail if concurrency is missing.

4. **Generic Server Usage**:
   - Use the pre-provided `genserver` module for process loops and sync messaging (not Erlang’s built-in `gen_server`).

5. **Debugging**:
   - Dump variable contents to the console: `io:fwrite("~p~n", [S])`.
   - Clean compiled files: `make clean`.
   - Start server + two clients in one command:
     ```bash
     make && erl -eval "cchat:server(), cchat:client(), cchat:client()."
     ```
   - Run a single test (e.g., `message_throughput_test`):
     ```bash
     make && erl -eval "eunit:test({test, test_client, message_throughput_test}), halt()."
     ```

6. **`badarg` Errors**:
   - Often occurs when sending messages to an unregistered process (ensure the server is running and registered to the correct atom).


## Distinction Assignment (2 Points)
### Task
Disallow duplicate nicks when users run `/nick newnick`:
- Check if the new nick is already in use before allowing the change.
- Return the error atom `nick_taken` if the nick is occupied.

### Exceptions
- Do not handle collisions for initial random nicks (assigned to clients by default).
- Do not handle collisions between clients that have not yet communicated with the server.


## Instructions for Terminal-based Interface
### 1. Start the Server
```bash
erl -sname server -eval "cchat:server()."
```

### 2. Start a Client (New Terminal)
```bash
erl -sname client_$RANDOM -remsh server@$(hostname -s)
```
This connects to the server’s Erlang VM (prompt will change to `(server@[hostname]) 1>`).

### 3. Launch the Terminal Client
```erlang
(server@[hostname]) 1> cchat:client_tui().
```
- You will see:
  - Server name (e.g., `shire`).
  - Your default nick (e.g., `client_12345`).
  - Initial joined channel (e.g., `#foo`).

### 4. Terminal Client Features
- Join multiple channels (e.g., `/join #bar`).
- The input prompt shows joined channels (e.g., `#foo|#bar>`).
- Messages are sent to the **rightmost channel** in the prompt.
- Cycle between channels by pressing `Enter` with no input.

### Example Workflow
```
#foo> /join #bar
* Joined #bar
#foo|#bar> hello bar!
[#bar] client_12345> hello bar!
#bar|#foo> % Press Enter (cycle channels)
#foo|#bar> hello foo!
[#foo] client_12345> hello foo!
```

Report any bugs in the terminal interface to the course staff.