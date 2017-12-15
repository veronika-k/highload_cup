/**
  * Created by inoquea on 14.12.17.
  */

import java.io.{ IOException, FileOutputStream, FileInputStream, File }
import java.util.zip.{ ZipEntry, ZipInputStream }


trait Unzip {

  val INPUT_ZIP_FILE: String = "/tmp/data/data.zip";
  val OUTPUT_FOLDER: String = "/tmp/data/unpacked/";

  def unZipIt(zipFile: String, outputFolder: String): Unit = {

    val buffer = new Array[Byte](1024)

    try {

      //output directory
      val folder = new File(OUTPUT_FOLDER);
      if (!folder.exists()) {
        folder.mkdir();
      }

      //zip file content
      val zis: ZipInputStream = new ZipInputStream(new FileInputStream(zipFile));
      //get the zipped file list entry
      var ze: ZipEntry = zis.getNextEntry();

      while (ze != null) {

        val fileName = ze.getName();
        val newFile = new File(outputFolder + File.separator + fileName);

        System.out.println("file unzip : " + newFile.getAbsoluteFile());

        //create folders
        new File(newFile.getParent()).mkdirs();

        val fos = new FileOutputStream(newFile);

        var len: Int = zis.read(buffer);

        while (len > 0) {

          fos.write(buffer, 0, len)
          len = zis.read(buffer)
        }

        fos.close()
        ze = zis.getNextEntry()
      }

      zis.closeEntry()
      zis.close()

    } catch {
      case e: IOException => println("exception caught: " + e.getMessage)
    }

  }
}
