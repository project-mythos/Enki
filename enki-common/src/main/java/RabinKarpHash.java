package Enki;

import java.util.Arrays;


public class RabinKarpHash  {

    private int B = 31;

    private int BtoN = 1;

    private int h, k, l = 0;

    private byte[] buf;

    
    public void reset() {
        BtoN = 1;
        h = k = l = 0;
    }

    
    public void init(byte[] bytes) {
        init(bytes, 0, bytes.length);
    }

    
    public void init(byte[] bytes, int off, int len) {
        reset();
        buf = Arrays.copyOfRange(bytes, off, len);
        l = len;
        for (int i = 0; i < l; i++) BtoN *= B;
        for (int i = 0; i < l; i++) h = B * h + ByteHash.array[buf[i] & 0xff];
    }

    
    public void roll(byte in) {
        h = B * h + ByteHash.array[in & 0xff] - BtoN * ByteHash.array[buf[k] & 0xff];
        buf[k] = in;
        k = (k == l - 1) ? 0 : k + 1;
    }

    
    public int getValue() {
        return h;
    }

}
