package utils

import java.io.File


/**
 * For caching domain objects.
 */
trait Cache {

  /**
   * Returns the object cached in a .csv file located by the given filepath.
   * @param filepath Filepath location of cache .csv file.
   * @return Object.
   */
  protected def fromCacheCSV(filepath: String): Any

  /**
   * Saves the given object to the cache .csv format at the give filepath.
   * @param o Object to cache.
   * @param filepath Filepath location of cache .csv file.
   */
  protected def toCacheCSV(o: Any, filepath: String): Unit

  /**
   * Returns the cache filepath for the object with the given id.
   * @param id Id of object.
   * @return Filepath to cache .csv file of object.
   */
  protected def getCachedFilepath(id: Any): String

  /**
   * Checks whether the object with the given id has a cache .csv file already generated.
   * @param id Id of object.
   * @return True if a cache .csv file exists for the object, false otherwise.
   */
  protected def inCache(id: Any): Boolean = new File(this.getCachedFilepath(id)).exists()

  /**
   * Loads the object with the given id from the cache. The object must already have a cache copy saved as a .csv file.
   * @param id Id of object.
   * @return The object.
   */
  protected def fromCache(id: Any): Any = this.fromCacheCSV(this.getCachedFilepath(id))

  /**
   * Saves the object with the given id to the cache. (Overwrites any existing cache copy saved as a .csv file.)
   * @param id Id of object.
   * @param o Object.
   */
  protected def toCache(id: Any, o: Any): Unit = this.toCacheCSV(o, this.getCachedFilepath(id))

  /**
   * Deletes the cached .csv file for the object with given id.
   * @param id Id of object.
   */
  protected def deleteFromCache(id: Any): Unit = if (this.inCache(id)) new File(this.getCachedFilepath(id)).delete()

  /**
   * Clears the cache.
   */
  protected def clearCache: Unit

}
