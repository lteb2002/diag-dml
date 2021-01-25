package com.reremouse.ju4ja.parser;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Ju4jaParser {


    public static Double[] parseStringAs1DArray(String str) {
        List<Double> ass = new ArrayList();
        try {
            String s1= str.trim().replace("[","").replace("]","");
            String[] ss=s1.split(",");
            for (String s:ss) {
                ass.add(Double.parseDouble(s.trim()));
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        Double[] arr = new Double[ass.size()];
        return ass.toArray(arr);
    }

    @Test
    public void test(){
        String s="[8.00001, 4.00001, 9.0713e-16, 18.0, 7.69078e-21, 7.10381e-15]";
        Double[] arr=parseStringAs1DArray(s);
        System.out.println(Arrays.toString(arr));
    }


}
