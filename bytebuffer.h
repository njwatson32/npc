#ifndef __BYTE_BUFFER_HEADER__
#define __BYTE_BUFFER_HEADER__

#include <string>
#include <vector>

class ByteBuffer {
private:
  std::vector<unsigned char> buffer;
  std::vector<unsigned char>::const_iterator it;

public:
  ByteBuffer() : it(buffer.begin()) { }
  // Initial capacity in bytes
  ByteBuffer(int initialCapacity) {
    buffer.reserve(initialCapacity);
    it = buffer.begin();
  }
  // The input here must have been produced by ByteBuffer::Serialized()
  ByteBuffer(char *serialized);

  // Returned byte array must be deleted
  char *Serialized();
  // The size of the array returned by ByteBuffer::Serialized()
  unsigned int SerializedSize() { return buffer.size() + sizeof(unsigned int); }

  void SerializeBool(bool x);
  void SerializeChar(char x);
  void SerializeShort(short x);
  void SerializeInt(int x);
  void SerializeLong(long x);
  void SerializeLongLong(long long x);
  void SerializeFloat(float x);
  void SerializeDouble(double x);
  void SerializeLongDouble(long double x);
  void SerializeString(const std::string &str);

  // Must be called if any Serialize methods have been called
  // between the constructor and deserialization
  void StartDeserialize() { it = buffer.begin(); }

  bool DeserializeBool();
  char DeserializeChar();
  short DeserializeShort();
  int DeserializeInt();
  long DeserializeLong();
  long long DeserializeLongLong();
  float DeserializeFloat();
  double DeserializeDouble();
  long double DeserializeLongDouble();
  std::string DeserializeString();
};

#endif // __BYTE_BUFFER_HEADER__
