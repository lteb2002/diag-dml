package com.reremouse.dml.svd;

import com.reremouse.dml.model.RereSVDResult;
import com.reremouse.weka.util.TimeUtil;
import com.reremouse.weka.util.WekaUtil;
import java.io.File;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.linear.SingularValueDecomposition;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ArffLoader;
import weka.core.converters.ArffSaver;

/**
 *
 * @author RereMouse
 */
public class RereSVD {

    public static String ext = ".svd";
    public static String fileExt = ".arff";
    public static String fileBase = "H:\\dml_experiments\\";
    public static String svdedExt = "_svd";

    /**
     *
     * @param name
     * @return
     */
    public Instances buildWekaDataSet(String name) {

        String filePath2 = fileBase + name + fileExt;
        System.out.println("File:" + filePath2);
        //RereDataUtil u = new RereDataUtil();
        int i1 = 0;
        int i2 = 0;
        int i3 = 0;
        try {
            File file2 = new File(filePath2);
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
     * @param name
     * @param ifNewSVD 是否强制重新执行SVD
     * @return
     */
    public RereSVDResult performSVD(String name, boolean ifNewSVD) {
        Instances ins = this.buildWekaDataSet(name);
        RealMatrix matrix = WekaUtil.buildDataMatrix(ins);
        RereSVDResult res = this.performSVD(name, matrix, ifNewSVD);
        return res;
    }

    /**
     *
     * @param name
     * @param ifNewSVD 是否强制重新执行SVD
     * @return
     */
    public RereSVDResult performSVD(String name, RealMatrix matrix, boolean ifNewSVD) {
        RereSVDResult res = null;
        String objFile = fileBase + "obj\\" + name + ext;
        File obj = new File(objFile);
        if (obj.exists() && !ifNewSVD) {
            res = RereSVDResult.readFromFile(name);
            System.out.println("SVD information is read from file.");

        } else {
            try {
                long begin = System.currentTimeMillis();
                //奇異值分解
                SingularValueDecomposition svd = new SingularValueDecomposition(matrix.transpose());
                double svs[] = svd.getSingularValues();
                //获得右特征向量
                RealMatrix v = svd.getV();
                RealMatrix u = svd.getU();
                long end = System.currentTimeMillis();
                long inteval = end - begin;
                res = new RereSVDResult(name, u, svs, v, inteval);
                res.save();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        if (res != null) {
            long len = res.getTimeLength();
            System.out.println("SVD time:" + TimeUtil.wrapperTimeLen(len) + "," + len);
        }
        return res;
    }

    /**
     *
     * @param res
     * @param dim 投影的维数
     * @return
     */
    public RealMatrix buildProjection(RereSVDResult res, int dim) {
        try {
            double[] svs = res.getS();
            //获得右特征向量
            RealMatrix u = res.getU();
            System.out.println(u.getRowDimension()+":"+u.getColumnDimension());
            //获得右特征向量的主特征向量
            RealMatrix um = u.getSubMatrix(0, u.getColumnDimension() - 1, 0, dim - 1);
            System.out.println("---------------------------------------------------------------------------");
            //获得将向量向低维投影的矩阵，胖宽矩阵
            RealMatrix p = um.transpose();
            //System.out.println(p);
            if (svs != null) {
                for (double sv : svs) {
                    System.out.println(sv);
                }
            }
            return p;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     *
     * @param res
     * @return
     */
    public RealMatrix buildProjection(RereSVDResult res) {
        try {
            double[] svs = res.getS();
            if (svs != null) {
                int dim = svs.length;
                System.out.println("The selected dimension is:" + dim);
                return this.buildProjection(res, dim);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     *
     * @param name
     * @param matrix
     * @param p
     */
    public void exportReducedData(String name, RealMatrix matrix, RealMatrix p) {
        String path1 = fileBase + name + fileExt;
        String path2 = fileBase + name + svdedExt + fileExt;
        try {
            System.out.println("---------------------------------------------------------------------------");

            File file = new File(path1);
            Instances ins = null;
            ArffLoader loader = new ArffLoader();
            loader.setFile(file);
            ins = loader.getDataSet();
            ins.setClassIndex(ins.numAttributes() - 1);
            int rows = ins.numInstances();

            Instances struct = loader.getStructure();
            int dim = p.getRowDimension();

            struct = WekaUtil.rebuildStructure(struct, dim);


            ArffSaver saver = new ArffSaver();
            saver.setFile(new File(path2));
            saver.setRetrieval(saver.INCREMENTAL);
            saver.setStructure(struct);

            //System.out.println("p:"+p.getRowDimension()+","+p.getColumnDimension());

            for (int i = 0; i < rows; i++) {
                RealMatrix data = matrix.getRowMatrix(i).transpose();
                RealMatrix rowT = p.multiply(data);
                double[] dx = rowT.getColumn(0);
                Instance in = new DenseInstance(dx.length + 1);
                int cla = (int) ins.instance(i).classValue();
                for (int j = 0; j < dx.length; j++) {
                    in.setValue(j, dx[j]);
                }
                in.setValue(dx.length, cla);
                saver.writeIncremental(in);
            }
            saver.getWriter().close();
            //pw.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 执行SVD的整个流程
     *
     * @param name
     */
    public static void performSequence(String name) {
        RereSVD svd = new RereSVD();
        Instances ins = svd.buildWekaDataSet(name);
        RealMatrix matrix = WekaUtil.buildDataMatrix(ins);
        RereSVDResult res = svd.performSVD(name, matrix, true);
        RealMatrix p = svd.buildProjection(res);
        svd.exportReducedData(name, matrix, p);
    }

    /**
     *
     * @param args
     */
    public static void main(String[] args) {
        String[] names = {"mnist"};
        for (String name : names) {
            RereSVD.performSequence(name);
        }
    }

}
