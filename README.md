# Installation guide for erlang chat test

## Installation on Ubuntu

### 1. Make sure git is installed. https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

### 2. Make sure erlang is installed. http://www.erlang.org/downloads

### 3. Clone the repository and compile the sources.

> git clone https://github.com/StartSWest/erlang_chat_test.git

> cd erlang_chat_test/

> ./compile

### 4. Editing configurations

#### Server configuration

In erlang distributed system two different distributed nodes have different configurations.
Two configurations files are provided in this example for main and backup servers.

The 'main.config' configuration setups the main server but also sets the option {sync_nodes_optional, ['backup_server@192.168.1.103']} to the backup server node address. A small change has to be made for 'backup.config'
file to set the {sync_nodes_optional, ['main_server@192.168.1.103']} to the main server node address.

For editing configurations type:

> gedit server/config/main.config

> gedit server/config/backup.config

Edit main and backup servers name/address to match your system:

{distributed, [{chat_server, ['main_server@192.168.1.103', {'backup_server@192.168.1.103'}]}]},

{sync_nodes_optional, ['backup_server@192.168.1.103']},

This uses fully qualified node names but you can use short-names too.
See client/config/client.config to change the 'server_name_type' to allow the client to connect to the server using
short-names.

#### Client configuration

> gedit client/config/client.config

Make {server_name, 'main_server@192.168.1.103'} matches your main or backup server node address. 

This option {server_name_type, longnames} specifies if you are using fully qualified names or short-names in the
server.

The following {server_cookie, 'secure123'} defines the cookie that will be used to connect to the server. See
section: 5. Starting the servers.

### 5. Starting the main and backup servers

In order to create multiple terminals/consoles the command 'gnome-terminal -- <command>' is used inside the following
bash files.

> ./serve-main

> ./serve-backup

Before executing this two commands first edit them to change the server node address to match your current system.

> gedit serve-main

Change the -name main_server@192.168.1.103 parameter to match your system server address. You can use -sname to use
short-names. See 'server_name_type' in client/config/client.config to allows clients to connect to the server using
short-names.

The same happens with server-backup script.

> gedit serve-main

Edit to mach your system backup server address.

### 5. Starting the clients

Type the following many times to get as many clients as you want.

> ./start-client

