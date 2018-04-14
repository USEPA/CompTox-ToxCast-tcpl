/*

patch_invitrodb.sql 

Create and populate tables mc4_methods and mc4_aeid in an existing invitrodb
instance.


"Tox-95" added flexibility to the toxcast pipeline (r package tcpl) in the
calulation and selection of threshold values for hit calling. These additions
require addition of two new tables to the mysql schema. The first (mc4_methods)
includes the definition of the methods for calculating baselines and the mapping
to relevant r functions. The second (mc4_aeid) maps these methods to assay
component endpoints in the database.

Patrick McMullen
pmcmullen@scitovation.com


*/




/*SET @sql_mode =
 * 'ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,ERROR_FOR_DIVISION_BY_ZERO,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION'
 * */;




/* 
CREATE TABLE mc4_methods
*/
DROP TABLE IF EXISTS `mc4_methods`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc4_methods` (
  `mc4_mthd_id` int(11) NOT NULL AUTO_INCREMENT,
  `mc4_mthd` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `desc` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`mc4_mthd_id`),
  UNIQUE KEY `mc4_method` (`mc4_mthd`)
) ENGINE=MyISAM AUTO_INCREMENT=3 DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;


/* 
Populate mc4_methods with relevant functions for calculating bmad
*/
LOCK TABLES `mc4_methods` WRITE;
/*!40000 ALTER TABLE `mc4_methods` DISABLE KEYS */;
INSERT INTO `mc4_methods` VALUES (1,'bmad.aeid.lowconc.twells','bmad based on two lowest concentration of treatment wells',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,'spendse.scitovation'),(2,'bmad.aeid.lowconc.nwells','bmad based on two lowest concentration of nwells',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,'spendse.scitovation');
/*!40000 ALTER TABLE `mc4_methods` ENABLE KEYS */;
UNLOCK TABLES;


/* 
CREATE TABLE mc4_aeid
*/
DROP TABLE IF EXISTS `mc4_aeid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc4_aeid` (
  `mc4_mthd_id` int(11) NOT NULL,
  `aeid` int(11) NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`mc4_mthd_id`,`aeid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;



/* 
Populate mc4_aeid for all existing assay_component_endpoint.aeid values. Assign
mc4_mthd_id = 1 for all existing aeids, which corresponds with the olde way of
assigning bmad
*/
LOCK TABLES `mc4_aeid` WRITE, `assay_component_endpoint` READ;
/*!40000 ALTER TABLE `mc4_aeid` DISABLE KEYS */;
INSERT INTO `mc4_aeid`  (mc4_mthd_id, aeid, created_date, modified_date, modified_by)
SELECT 1, aeid, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 'patrick.patrick.patrick' FROM `assay_component_endpoint`;
/*!40000 ALTER TABLE `mc4_aeid` ENABLE KEYS */;
UNLOCK TABLES;
