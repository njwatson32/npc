#include "bytebuffer.h"

#include <cstdlib>
#include <ctime>
#include <string>
#include <vector>

#define BYTE 8

#define SERIALIZE2(TYPE, X)                             \
  for (unsigned int i = sizeof(TYPE); i > 0; i--)       \
    buffer.push_back((X >> ((i-1) * BYTE)) & 0xff)

#define DESERIALIZE2(TYPE, X)                           \
  TYPE X = *it++;                                       \
  for (unsigned int i = 1; i < sizeof(TYPE); i++)       \
    (X <<= BYTE) += (*it++)

#define SERIALIZE(TYPE, X)                                      \
  unsigned char *bytes = reinterpret_cast<unsigned char *>(&X); \
  for (unsigned int i = 0; i < sizeof(TYPE); i++)               \
    buffer.push_back(bytes[i])

#define DESERIALIZE(TYPE, X)                            \
  unsigned char bytes[sizeof(TYPE)];                    \
  for (unsigned int i = 0; i < sizeof(TYPE); i++)       \
    bytes[i] = *it++;                                   \
  TYPE *_ptr = reinterpret_cast<TYPE *>(bytes);         \
  TYPE X = *_ptr

ByteBuffer::ByteBuffer(char *serialized) {
  unsigned int s = sizeof(unsigned int);
  unsigned char bytes[sizeof(unsigned int)];
  for (unsigned int i = 0; i < s; i++)
    bytes[i] = serialized[i];
  unsigned int *size = reinterpret_cast<unsigned int *>(bytes);
  buffer.reserve(*size);
  for (unsigned int j = s; j < *size + s; j++)
    buffer.push_back(serialized[j]);
  it = buffer.begin();
}

char *ByteBuffer::Serialized() {
  unsigned int s = sizeof(unsigned int);
  unsigned int size = buffer.size();
  unsigned char *bytes = reinterpret_cast<unsigned char *>(&size);
  char *serialized = new char[buffer.size() + s];
  for (unsigned int i = 0; i < s; i++)
    serialized[i] = bytes[i];
  for (unsigned int j = 0; j < buffer.size(); j++)
    serialized[j + s] = buffer[j];
  return serialized;
}  

void ByteBuffer::SerializeBool(bool x) {
  buffer.push_back(x);
}

void ByteBuffer::SerializeChar(char x) {
  buffer.push_back(x);
}

void ByteBuffer::SerializeShort(short x) {
  SERIALIZE(short, x);
}

void ByteBuffer::SerializeInt(int x) {
  SERIALIZE(int, x);
}

void ByteBuffer::SerializeLong(long x) {
  SERIALIZE(long, x);
}

void ByteBuffer::SerializeLongLong(long long x) {
  SERIALIZE(long long, x);
}

void ByteBuffer::SerializeFloat(float x) {
  SERIALIZE(float, x);
}

void ByteBuffer::SerializeDouble(double x) {
  SERIALIZE(double, x);
}

void ByteBuffer::SerializeLongDouble(long double x) {
  SERIALIZE(long double, x);
}

void ByteBuffer::SerializeTime_t(time_t x) {
  SERIALIZE(time_t, x);
}

void ByteBuffer::SerializeSize_t(size_t x) {
  SERIALIZE(size_t, x);
}

void ByteBuffer::SerializeString(const std::string &str) {
  for (std::string::const_iterator i = str.begin();
       i != str.end(); ++i)
    buffer.push_back(*i);
  buffer.push_back('\0');
}

bool ByteBuffer::DeserializeBool() {
  return *it++ > 0;
}

char ByteBuffer::DeserializeChar() {
  return *it++;
}

short ByteBuffer::DeserializeShort() {
  DESERIALIZE(short, x);
  return x;
}

int ByteBuffer::DeserializeInt() {
  DESERIALIZE(int, x);
  return x;
}

long ByteBuffer::DeserializeLong() {
  DESERIALIZE(long, x);
  return x;
}

long long ByteBuffer::DeserializeLongLong() {
  DESERIALIZE(long long, x);
  return x;
}

float ByteBuffer::DeserializeFloat() {
  DESERIALIZE(float, x);
  return x;
}

double ByteBuffer::DeserializeDouble() {
  DESERIALIZE(double, x);
  return x;
}

long double ByteBuffer::DeserializeLongDouble() {
  DESERIALIZE(long double, x);
  return x;
}

time_t ByteBuffer::DeserializeTime_t() {
  DESERIALIZE(time_t, x);
  return x;
}

size_t ByteBuffer::DeserializeSize_t() {
  DESERIALIZE(size_t, x);
  return x;
}

std::string ByteBuffer::DeserializeString() {
  std::vector<unsigned char>::const_iterator first = it;
  while (*it != '\0')
    ++it;
  return std::string(first, it++);
}
