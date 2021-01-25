package com.reremouse.util.test;


import breeze.linalg.DenseVector;
import com.github.fommil.netlib.*;

/**
 * 测试加载的BLAS库
 */
public class NetLibTest {

    public static void main(String[] args) {
        NativeRefBLAS.getInstance();
        NativeRefLAPACK.getInstance();
        NativeRefARPACK.getInstance();
        DenseVector v = new DenseVector(new Double[]{1.0, 2.0, 3.0, 4.0});
        System.out.println(v);
    }

}
