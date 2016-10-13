- [X] Changing the nickname
- [X] Asking for the nickname
- [ ] Leaving a chat room
- [ ] Writing messages in a chat room
- [ ] Joining a chat room
- [X] Disconnecting from the server
- [X] Connecting to the server

Todo-further:
- [ ] Disconnect from chat rooms


Connecting to the server

    When the user writes the command /connect Server, the GUI sends the message {connect, Server} to the client process, where variable Server denotes the name of the chat server chosen by the user.

    Atom user_already_connected is used when the user tried to connect but it is already connected to the server. Atom server_not_reached is returned when the server process cannot be reached for any reason.

Disconnecting from the server

    When the user writes the command /disconnect, the GUI sends the message disconnect to the client process. It is possible to disconnect from a server only if the user has left all the chat rooms (i.e. channels) first.

    Atom user_not_connected is returned when a user tries to disconnect from a server where he/she is not connected to. Atom leave_channels_first is returned when a user tries to disconnect from a server without leaving the chat rooms first. Lastly, atom server_not_reached is used when the server process cannot be reached for any reason.

Joining a chat room

    To join a chat room, users write the command /join Chatroom, where Chatroom is a string starting with "#", e.g., "#hobbits". The GUI sends the message {join, Chatroom}.

    Internally, if the chat room does not exists in the server side, the server process will create it. We assume that, once created, chat rooms are never destroyed, i.e., they will always exist as long as the server runs. Bear in mind that only users who have joined a chat room can write messages on it.

    Atom user_already_joined is return when a user tries to join to the same channel again.

Writing messages in a chat room

    When the user is in a chat room and writes an string (not started with /), the GUI sends the message {msg_from_GUI, Chatroom, String} where variable Chatroom contains the name of the channel as an string (e.g. "#hobbits") and variable String stores the typed string (e.g. "hello fellow hobbits").

    Atom user_not_joined is returned when a user tries to write a message in a channel that he/she has not joined.

Leaving a chat room

    When the user types /leave in a chat room, the GUI sends the message {leave, Chatroom}, where variable Chatroom contains the name of the chat room.

    Atom user_not_joined is returned when a user tries to leave a channel that he/she has not joined.

Asking for the nickname

    When the user writes the command /whoami, the GUI sends the message whoami to the client process. The client should respond with the nick as a string (instead of the atom ok). There are no errors to report in this case.

Changing the nickname

    When the user writes the command /nick Name, the GUI sends the message {nick, Name} to the client process. Variable Name contains the new chosen nickname for the user.

    To make things easier, you only need to support changing nick when the user is disconnected. Return atom user_already_connected if the user attempts to change nick when connected.

Optional: If you wish to implement changing the nick when connected, you should return atom nick_taken when trying to change to nick that is already taken.
