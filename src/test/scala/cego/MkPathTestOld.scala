package cego

class MkPathTestOld extends RBTSpec{
  it should "create correct path indentions" in {
    assert(MkPath().dir("III1234") == List("1234", "3_", "34"))
    assert(MkPath().dir("IIIIID4") == List("4", "0_", "04"))
    assert(MkPath(3).dir("III1234") == List("1234", "2__", "23_", "234"))
    assert(MkPath(6).dir("III1234") == List("1234", "0_____", "00____", "001___", "0012__", "00123_", "001234"))
    assert(MkPath(3).dir("12345678")== List("12345678", "6__", "67_", "678"))
  }
}
