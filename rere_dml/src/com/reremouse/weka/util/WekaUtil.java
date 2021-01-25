package com.reremouse.weka.util;

import java.io.File;
import java.util.Arrays;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import weka.core.Attribute;
import static weka.core.Debug.DBO.p;
import weka.core.DenseInstance;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ArffLoader;
import weka.core.converters.ArffSaver;

/**
 *
 * @author RereMouse
 */
public class WekaUtil {

    /**
     * 重新构建数据头
     *
     * @param struct 数据头
     * @param dim 维度
     * @return
     */
    public static Instances rebuildStructure(Instances struct, int dim) {
        int an = struct.numAttributes();
        Attribute label = struct.attribute(an - 1);
        System.out.println("Total attr: " + an);
        for (int i = 0; i < an; i++) {
            try {
                //System.out.println("Deleting at " + i);
                struct.deleteAttributeAt(0);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        for (int i = 1; i <= dim; i++) {
            Attribute att = new Attribute("x" + String.valueOf(i));
            struct.insertAttributeAt(att, i - 1);
        }
        int num = label.numValues();

        FastVector fv = new FastVector();
        for (int i = 0; i < num; i++) {
            fv.addElement(String.valueOf(i));
        }
        Attribute label2 = new Attribute("label", fv);
        struct.insertAttributeAt(label2, dim);
        struct.setClassIndex(dim);

        System.out.println(struct);
        return struct;
    }

    /**
     *
     * @param ins
     * @return
     */
    public static RealMatrix buildDataMatrix(Instances ins) {
        int rows = ins.numInstances();
        int cols = ins.numAttributes();
        System.out.println("Row:" + rows + ", col:" + (cols - 1));
        RealMatrix matrix = new Array2DRowRealMatrix(rows, (cols - 1));
        //MahalanobisUtil.init(ins);
        //MahalanobisUtil.tellAll();
        for (int i = 0; i < ins.numInstances(); i++) {
            Instance in = ins.instance(i);
            double value[] = in.toDoubleArray();
            value = Arrays.copyOfRange(value, 0, cols - 1);
            matrix.setRow(i, value);
            //RereDataUtil.showArray(value);
            int label = (int) in.classValue();
        }
        return matrix;
    }

    /**
     * 从文件中加载weka实例
     *
     * @param file
     * @return
     */
    public static Instances buildWekaDataSet(String file) {
        int i1 = 0;
        int i2 = 0;
        int i3 = 0;
        try {
            File file2 = new File(file);
            Instances ins = null;
            ArffLoader loader = new ArffLoader();
            loader.setFile(file2);
            ins = loader.getDataSet();
            ins.setClassIndex(ins.numAttributes() - 1);
            return ins;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     *
     * @param ins
     * @param path
     */
    public static void exportInstances(Instances ins, String path) {
        try {
            ArffSaver saver = new ArffSaver();
            saver.setFile(new File(path));
            //saver.setRetrieval(saver.INCREMENTAL);
            saver.setInstances(ins);
            saver.writeBatch();
//            saver.getWriter().close();
            //pw.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
