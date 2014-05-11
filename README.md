Nick's Packet Compiler

+-------+
| BUILD |
+-------+

To re-compile, just run 'make'. You will need the Haskell platform.

+------------+
| INVOCATION |
+------------+

The tool may be invoked using either 'npc --all [--inc=FILE]' or
'npc [--inc=FILE] packet1 [packet2...]'. The --all flag (short option -a)
will run the tool on all files in the directory with a .packet extension.
The --inc=FILE flag (short option -iFILE) specifies the inclusion file for
user-defined classes (see USER CLASSES below). It is necessary if and only
if your packets contain user-defined classes.

Instead of using --all, you may invoke npc by explicitly naming the packets
you wish to compile.

Help on the npc command can be found by invoking npc with no arguments.

+--------+
| FORMAT |
+--------+

The format of packets is as follows:

packet <packet_name> {
  <C_type> <field_name>;
  ...
}

Standard C++ naming rules apply. <packet_name> may not be Packet. Empty
packets are allowed.

Packets in the same .packet file will be in the same .h/.cpp files.

+---------------+
| ALLOWED TYPES |
+---------------+

Support is built-in for all primitives, size_t, time_t, (C++) strings, pairs,
vectors, lists, and maps. No pointers or references are allowed. Types may be
nested, e.g. map<string, vector<int>>.
If <C_type> is a user-defined class, that class must define
        - unsigned int SerializedSize() const
        - void Serialize(ByteBuffer &) const
        - void Deserialize(ByteBuffer &)

+------------+
| USER TYPES |
+------------+

For user-defined types, you will need to specify an inclusions file that
tells npc which header files to include for those classes. This should be
specified with the --inc=FILE option. The file itself should look like

class_name1:header_file1;
class_name2:header_file2;
...

Each class you use must be specified in the inclusions file, or a "Map.find"
error will be thrown.

+----------+
| COMMENTS |
+----------+

Block comments (/* ... */) are allowed outside and only outside
packet definitions. Nesting block comments is NOT allowed.

Single-line comments (// ...) are allowed inside and only inside packet
definitions. Single line comments are also allowed in the inclusions file.

Allowed (packet files):

  /* This packet is for requests.
     I like requests. */
  packet Request {
    // Here are the fields
    int status; // OK
    //bool debug;
    string name;
  }

Not allowed (packet files):

  // The best response ever
  packet Response {
    /* I don't want these now
    bool save;
    string filename;
    */
    int number;
  }

Allowed (inclusions file):

  MyClass:myclass.h // awesome
  //Foo:foo.h

Not Allowed (inclusions file):

  MyClass:myclass.h
  /*
  Foo:foo.h
  Bar:bar.h
  */

+--------------+
| KNOWN ISSUES |
+--------------+

None.

+-----------+
| CHANGELOG |
+-----------+

Version 1.6:
Added support for pairs.

Version 1.5:
Added TypeString method to Packet
Added virtual destructor to Packet, made ctor/dtor public
Added support for size_t and time_t

Version 1.4:
Added support for lists
Added sequence numbers

Version 1.3:
Added inclusions file.
Removed --main option.
Removed boolean err from Packet::DeserializePacket

Version 1.2:
Added more convenient deserialization
Reduced number of I/O operations
Fixed empty packet bug

Version 1.1:
Non-const accessors removed for primitives, since they're useless
Source file Npc.hs now has comments

Version 1.0:
Initial release