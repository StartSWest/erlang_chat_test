# Installation guide for erlang chat test

## Cross-Platform Installation

### 1. Pre-Requirements

- Make sure erlang is installed and in path. http://www.erlang.org/downloads

  NOTE: You can test it by typing *erl* in the terminal of your operating system.
 
### 2. Downloading the test and compiling the sources.

1. [Download it from here](https://github.com/StartSWest/erlang_chat_test/archive/master.zip) or just clone this repo.

2. Extract it on a desired folder.

3. Enter to the folder `erlang_chat_test-master` or `erlang_chat_test` if you clonned it using git clone. If you are using the shell just type the equivalent command for your system.

```
cd erlang_chat_test-master/
```

4. Build the test builder helper.

```
erl -make
```

5. Run the test builder helper

```
erl -s make_test
```

### 3. Editing configurations

#### Server configuration

In erlang distributed system two different distributed nodes have different configurations. Two configurations files are provided in this example for main and backup servers.

The `main.config` configuration setups the main server but also sets the option *{sync_nodes_optional, ['backup_server@192.168.1.103']}* to the backup server node address. A small change has to be made for `backup.config` file to set the *{sync_nodes_optional, ['main_server@192.168.1.103']}* to the main server node address.

For editing configurations for main and backup server open this files `main.config` and `backup.config` located under `server/config` directory.

Edit main and backup servers name/address to match your system:

*{distributed, [{chat_server, ['main_server@192.168.1.103', {'backup_server@192.168.1.103'}]}]},

 {sync_nodes_optional, ['backup_server@192.168.1.103']},*

This uses fully qualified node names but you can use short-names too. See `client.config` to change the *server_name_type* to allow clients to connect to the server using short-names.

#### Client configuration

To edit the client configuration, open the file `client.config` located under `client/config` folder.

Make sure *{server_name, 'main_server@192.168.1.103'}* matches your main or backup server node address.

This option *{server_name_type, longnames}* specifies if you are using fully qualified names or short-names in the server.

The following *{server_cookie, 'secure123'}* defines the cookie that will be used to connect to the server. See section: *4. Starting the servers.*

There are also other client configurations provided in this example, such as `clientb.config` to start a client that will connect to the backup server.

### 4. Starting the main and backup servers

Make sure you are in the root directory of the test `erlang_chat_test` and type this command into the terminal to start the main server.
Don't forget to change the parameter *-name main_server@192.168.1.103* to match your system address.

```
erl -noshell -name main_server@192.168.1.103 -pa server -config server/config/main -setcookie secure123 -s chat_server
```

You need to open more terminals to be able to have many instances of clients and servers, always stay in the root folder of the test in all terminal before running any command.

This will run the backup server. Remember to edit the parameter -sname to match your system address.

```
erl -noshell -name backup_server@192.168.1.103 -pa server -config server/config/backup -setcookie secure123 -s chat_server

```

It is also possible to use *-sname* in both servers to configure short-name address type. See *server_name_type* in `client.config` to allow clients to connect to the server using short-names.

### 5. Starting the clients

Open as many terminals as you want in the root directory of the test and try the following commands one for each terminal get as many clients A and B as you want. 'A' clients will try to connect to the main server according with its configuration in `client.config`. 'B' clients will connect to the backup server and will use `clientb.config` configuration file.

```
erl -pa client -config client/config/client -s chat_client
```

```
erl -pa client -config client/config/clientb -s chat_client
```

### 6. Tests and coverage

Before running the tests make sure you have zero clients connected to the servers, you must close all terminals that are running any client or just use `logout().` on all of them. You should also make sure you have both main and backup servers running, see Section *4. Starting the main and backup servers* then open two shells one for Client A and the other for Client B then type in one shell 

```
erl -pa client -config client/config/client -s chat_client -s tests_helper auto_login_for_tests
```

And in the other shell type:

```
erl -pa client -config client/config/clientb -s chat_client -s tests_helper run_tests
```

The tests will start and hopefully run successfully. Both clients will try to login to both servers, one will login to main server and the other to the backup server and they will be able to interact with each other, create chat, groups and send messages. This also test distribution.

#### Coverage

You can see the test coverage by opening the web page generated by the test framework after the test is finished located in `tests_results/index.html`. You must click on > *client.tests.tests_SUITE* < link, then on > *Coverage log* < link.

### 7. Dialyzer and xref

First of all, you need to build the plt database:

```
dialyzer --build_plt --output_plt dialyzer.plt --apps erts kernel stdlib

dialyzer --add_to_plt --plt dialyzer.plt -r ./client

dialyzer --add_to_plt --plt dialyzer.plt -r ./server```
```

Then you can run dialyzer checks:

```
dialyzer --plt dialyzer.plt -r ./client
dialyzer --plt dialyzer.plt -r ./server;
```

Running xref is quite simple too:

```
erl -eval 'xref:start(s), xref:add_directory(s, "./client"), xref:add_directory(s, "./server")'
```

## Installation on Ubuntu 18.04

### 1. Pre-Requirements

- Make sure git is installed. https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

- Make sure erlang is installed. http://www.erlang.org/downloads

- gedit
```
sudo apt install gedit
```

- gnome-terminal
```
sudo apt install gnome-terminal
```

### 2. Cloning the repository and compiling the sources.

```
git clone https://github.com/StartSWest/erlang_chat_test.git
```
```
cd erlang_chat_test/
```
```
./compile
```

You can use the following commands to interact with the test.

`./compile` will compile it.

`./serve` will start both main and backup servers.

`./start-client` will start a client that will connect to main server according to its default configuration.

`./start-clientb` will start a client that will connect to backup server according to its default configuration.

`./run-tests` will run all tests.

`./dialyzer` for dialyzer analysis.

`./xref` for xref analysis.

# How it works

In the current architecture there is no data persistence if any of the servers goes down. A second server will take over the application and start it clean without no data. The users get disconnected from the server if they were connected to the server that went down. A second `clientb.config` configuration is provided in this example to start clients that will connect to the backup server. For this kind of setup to work as expected a proxy must be configured in front of the servers so the clients always connect to the same address regardless of which server will take over when the other goes down.

There is a second setup I can think of and that is manually starting the applications in both main and server nodes. The last server to start ask to one of the running server to sync the data, also it could be possible to start *pg2* global progress group to join the new server so any subsequent messages sent from the users can be send to all servers in the group. I didn't implement this way because of the simplexes and elegance of the first one which is native erlang distribution with global processes.

#### Available chat client commands / functions.

- *login(User).*: Logs in a user to the server.

- *chat_with(TargetUser).*: Sends a chat request to a target user. Returns a chat id that both users can use to chat
   with each other.

- *send(ChatId, Message).*: Sends a message to the other user involved in the chat request created by 'chat_with'.

- *sendg(GroupName, Message).*: Sends a message all users in the group.

- *create_group(GroupName).*: Creates a chat group to send messages to many users at the same time. See 'add_to_group'
  and 'sendg'.

- *add_to_group(GroupName, User).*: Adds a user to a chat group.

- *remove_from_group(GroupName, User).*: Removes a user from a chat group.

- *logout().*: Logs out the current user from the server.

- *help().*: Shows this help.
