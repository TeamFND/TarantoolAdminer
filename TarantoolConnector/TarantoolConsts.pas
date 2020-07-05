unit TarantoolConsts;

{$mode objfpc}{$H+}

interface

const
  IPROTO_CODE = $00;
  IPROTO_SYNC = $01;
  //replication keys (header)
  IPROTO_SERVER_ID = $02;
  IPROTO_LSN = $03;
  IPROTO_TIMESTAMP = $04;
  IPROTO_SCHEMA_ID = $05;

  IPROTO_SPACE_ID = $10;
  IPROTO_INDEX_ID = $11;
  IPROTO_LIMIT = $12;
  IPROTO_OFFSET = $13;
  IPROTO_ITERATOR = $14;
  IPROTO_INDEX_BASE = $15;

  IPROTO_KEY = $20;
  IPROTO_TUPLE = $21;
  IPROTO_FUNCTION_NAME = $22;
  IPROTO_USER_NAME = $23;

  IPROTO_SERVER_UUID = $24;
  IPROTO_CLUSTER_UUID = $25;
  IPROTO_VCLOCK = $26;
  IPROTO_EXPR = $27;
  IPROTO_OPS = $28;

  IPROTO_DATA = $30;
  IPROTO_ERROR = $31;

  IPROTO_GREETING_SIZE = 128;
  IPROTO_BODY_MAX_LEN = 2147483648;

  IPROTO_REQUEST_TYPE = $0;
  IPROTO_REQUEST_TYPE_OK = 0;
  IPROTO_REQUEST_TYPE_SELECT = 1;
  IPROTO_REQUEST_TYPE_INSERT = 2;
  IPROTO_REQUEST_TYPE_REPLACE = 3;
  IPROTO_REQUEST_TYPE_UPDATE = 4;
  IPROTO_REQUEST_TYPE_DELETE = 5;
  IPROTO_REQUEST_TYPE_CALL16 = 6;
  IPROTO_REQUEST_TYPE_AUTHENTICATE = 7;
  IPROTO_REQUEST_TYPE_EVAL = 8;
  IPROTO_REQUEST_TYPE_UPSERT = 9;
  IPROTO_REQUEST_TYPE_CALL = 10;
  IPROTO_REQUEST_TYPE_PING = 64;
  IPROTO_REQUEST_TYPE_JOIN = 65;
  IPROTO_REQUEST_TYPE_SUBSCRIBE = 66;
  IPROTO_REQUEST_TYPE_ERROR = 1 << 15;

  SPACE_SCHEMA = 272;
  SPACE_SPACE = 280;
  SPACE_INDEX = 288;
  SPACE_FUNC = 296;
  SPACE_VSPACE = 281;
  SPACE_VINDEX = 289;
  SPACE_VFUNC = 297;
  SPACE_USER = 304;
  SPACE_PRIV = 312;
  SPACE_CLUSTER = 320;

  INDEX_SPACE_PRIMARY = 0;
  INDEX_SPACE_NAME = 2;
  INDEX_INDEX_PRIMARY = 0;
  INDEX_INDEX_NAME = 2;

  ITERATOR_EQ = 0;
  ITERATOR_REQ = 1;
  ITERATOR_ALL = 2;
  ITERATOR_LT = 3;
  ITERATOR_LE = 4;
  ITERATOR_GE = 5;
  ITERATOR_GT = 6;
  ITERATOR_BITSET_ALL_SET = 7;
  ITERATOR_BITSET_ANY_SET = 8;
  ITERATOR_BITSET_ALL_NOT_SET = 9;
  ITERATOR_OVERLAPS = 10;
  ITERATOR_NEIGHBOR = 11;

implementation

end.