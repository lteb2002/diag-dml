package com.reremouse.dml.model;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.Date;
import java.util.UUID;

import com.reremouse.dml.svd.RereSVD;
import org.apache.commons.math3.linear.RealMatrix;

/**
 *
 * @author RereMouse
 */
public class RereSVDResult implements Serializable {

    private String id = UUID.randomUUID().toString();
    private String name;
    private RealMatrix v;
    private RealMatrix u;
    private double[] s;
    private long timeLength;
    private Date genTime=new Date();

    public RereSVDResult(String name, RealMatrix u, double[] s,RealMatrix v,Long timeLength) {
        this.name = name;
        this.v = v;
        this.u = u;
        this.s = s;
        this.timeLength=timeLength;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public RealMatrix getV() {
        return v;
    }

    public void setV(RealMatrix v) {
        this.v = v;
    }

    public RealMatrix getU() {
        return u;
    }

    public void setU(RealMatrix u) {
        this.u = u;
    }

    public double[] getS() {
        return s;
    }

    public void setS(double[] s) {
        this.s = s;
    }

    public long getTimeLength() {
        return timeLength;
    }

    public void setTimeLength(long timeLength) {
        this.timeLength = timeLength;
    }

    public Date getGenTime() {
        return genTime;
    }

    public void setGenTime(Date genTime) {
        this.genTime = genTime;
    }

    /**
     * 将本JAVA对象序列化
     *
     */
    public void save() {
        String path = RereSVD.fileBase+"obj\\" + name + RereSVD.ext;
        try {
            OutputStream os = new BufferedOutputStream(new FileOutputStream(path));
            ObjectOutputStream out = new ObjectOutputStream(os);
            out.writeObject(this);
            out.close();
            os.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 
     * @param name
     * @return 
     */
    public static RereSVDResult readFromFile(String name) {
        String newPath = RereSVD.fileBase+"obj\\" + name + RereSVD.ext;
        RereSVDResult res = null;
        try {
            //反序列化对象
            ObjectInputStream ins = new ObjectInputStream(new FileInputStream(newPath));
            res = (RereSVDResult) ins.readObject();    //读取customer对象
            ins.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return res;
    }

}
