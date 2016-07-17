--
-- Table structure for table `assay`
--

DROP TABLE IF EXISTS `assay`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `assay` (
  `aid` int(11) NOT NULL AUTO_INCREMENT,
  `asid` int(11) NOT NULL,
  `assay_name` varchar(255) COLLATE latin1_general_ci NOT NULL,
  `assay_desc` text COLLATE latin1_general_ci,
  `timepoint_hr` double DEFAULT NULL,
  `organism_id` int(11) DEFAULT NULL,
  `organism` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `tissue` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `cell_format` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `cell_free_component_source` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `cell_short_name` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `cell_growth_mode` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `assay_footprint` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `assay_format_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `assay_format_type_sub` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `content_readout_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `dilution_solvent` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `dilution_solvent_percent_max` double DEFAULT NULL,
  PRIMARY KEY (`aid`),
  UNIQUE KEY `assay_name` (`assay_name`),
  KEY `assay_footprint` (`assay_footprint`),
  KEY `assay_format_type` (`assay_format_type`),
  KEY `assay_format_type_sub` (`assay_format_type_sub`),
  KEY `assay_source_id` (`asid`),
  KEY `cell_format` (`cell_format`),
  KEY `cell_free_component_source` (`cell_free_component_source`),
  KEY `cell_growth_mode` (`cell_growth_mode`),
  KEY `cell_short_name` (`cell_short_name`),
  KEY `content_readout_type` (`content_readout_type`),
  KEY `dilution_solvent` (`dilution_solvent`),
  KEY `organism_id` (`organism_id`),
  KEY `timepoint_hr` (`timepoint_hr`),
  KEY `tissue` (`tissue`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `assay`
--

LOCK TABLES `assay` WRITE;
/*!40000 ALTER TABLE `assay` DISABLE KEYS */;
/*!40000 ALTER TABLE `assay` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `assay_component`
--

DROP TABLE IF EXISTS `assay_component`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `assay_component` (
  `acid` int(11) NOT NULL AUTO_INCREMENT,
  `aid` int(11) NOT NULL,
  `assay_component_name` varchar(255) COLLATE latin1_general_ci NOT NULL,
  `assay_component_desc` text COLLATE latin1_general_ci,
  `assay_component_target_desc` text COLLATE latin1_general_ci,
  `parameter_readout_type` varchar(255) COLLATE latin1_general_ci DEFAULT 'single' COMMENT 'single parameter or multiple parameter',
  `assay_design_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `assay_design_type_sub` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `biological_process_target` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `detection_technology_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `detection_technology_type_sub` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `detection_technology` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `signal_direction_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `key_assay_reagent_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `key_assay_reagent` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `technological_target_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `technological_target_type_sub` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`acid`),
  UNIQUE KEY `assay_component_name` (`assay_component_name`),
  KEY `assay_design_type` (`assay_design_type`),
  KEY `assay_design_type_sub` (`assay_design_type_sub`),
  KEY `assay_id` (`aid`),
  KEY `biological_process_target` (`biological_process_target`),
  KEY `detection_technology` (`detection_technology`),
  KEY `detection_technology_type` (`detection_technology_type`),
  KEY `detection_technology_type_sub` (`detection_technology_type_sub`),
  KEY `key_assay_reagent` (`key_assay_reagent`),
  KEY `key_assay_reagent_type` (`key_assay_reagent_type`),
  KEY `intended_signal_direction` (`signal_direction_type`),
  KEY `parameter_readout_type` (`parameter_readout_type`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `assay_component`
--

LOCK TABLES `assay_component` WRITE;
/*!40000 ALTER TABLE `assay_component` DISABLE KEYS */;
/*!40000 ALTER TABLE `assay_component` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `assay_component_endpoint`
--

DROP TABLE IF EXISTS `assay_component_endpoint`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `assay_component_endpoint` (
  `aeid` int(11) NOT NULL AUTO_INCREMENT,
  `acid` int(11) NOT NULL,
  `assay_component_endpoint_name` varchar(255) COLLATE latin1_general_ci NOT NULL,
  `export_ready` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '0 is “initiated but not ready for release” and 1 is “ready for export to dashboard or other media”',
  `internal_ready` tinyint(1) DEFAULT '0',
  `assay_component_endpoint_desc` text COLLATE latin1_general_ci,
  `assay_function_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `normalized_data_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `analysis_direction` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `burst_assay` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `key_positive_control` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `signal_direction` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `intended_target_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `intended_target_type_sub` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `intended_target_family` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `intended_target_family_sub` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `fit_all` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`aeid`),
  UNIQUE KEY `assay_component_endpoint_name` (`assay_component_endpoint_name`),
  KEY `analysis_direction` (`analysis_direction`),
  KEY `assay_component_id` (`acid`),
  KEY `normalized_data_type` (`normalized_data_type`),
  KEY `assay_function_type` (`assay_function_type`),
  KEY `signal_direction` (`signal_direction`),
  KEY `internal_ready` (`internal_ready`),
  KEY `export_ready` (`export_ready`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `assay_component_endpoint`
--

LOCK TABLES `assay_component_endpoint` WRITE;
/*!40000 ALTER TABLE `assay_component_endpoint` DISABLE KEYS */;
/*!40000 ALTER TABLE `assay_component_endpoint` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `assay_component_map`
--

DROP TABLE IF EXISTS `assay_component_map`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `assay_component_map` (
  `acid` int(11) NOT NULL,
  `acsn` varchar(255) COLLATE latin1_general_ci NOT NULL,
  UNIQUE KEY `unique_id_source_name` (`acid`,`acsn`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `assay_component_map`
--

LOCK TABLES `assay_component_map` WRITE;
/*!40000 ALTER TABLE `assay_component_map` DISABLE KEYS */;
/*!40000 ALTER TABLE `assay_component_map` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `assay_reagent`
--

DROP TABLE IF EXISTS `assay_reagent`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `assay_reagent` (
  `arid` int(11) NOT NULL AUTO_INCREMENT,
  `aid` int(11) NOT NULL,
  `reagent_name_value` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `reagent_name_value_type` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `culture_or_assay` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`arid`),
  KEY `assay_id` (`aid`),
  KEY `culture_or_assay` (`culture_or_assay`),
  KEY `reagent_name_value` (`reagent_name_value`),
  KEY `reagent_name_value_type` (`reagent_name_value_type`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `assay_reagent`
--

LOCK TABLES `assay_reagent` WRITE;
/*!40000 ALTER TABLE `assay_reagent` DISABLE KEYS */;
/*!40000 ALTER TABLE `assay_reagent` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `assay_reference`
--

DROP TABLE IF EXISTS `assay_reference`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `assay_reference` (
  `reference_id` int(11) NOT NULL,
  `aid` int(11) NOT NULL,
  `citation_id` int(11) NOT NULL,
  PRIMARY KEY (`reference_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `assay_reference`
--

LOCK TABLES `assay_reference` WRITE;
/*!40000 ALTER TABLE `assay_reference` DISABLE KEYS */;
/*!40000 ALTER TABLE `assay_reference` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `assay_source`
--

DROP TABLE IF EXISTS `assay_source`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `assay_source` (
  `asid` int(11) NOT NULL AUTO_INCREMENT,
  `assay_source_name` varchar(255) COLLATE latin1_general_ci NOT NULL,
  `assay_source_long_name` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `assay_source_desc` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`asid`),
  KEY `assay_source_long_name` (`assay_source_long_name`),
  KEY `assay_source_name` (`assay_source_name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `assay_source`
--

LOCK TABLES `assay_source` WRITE;
/*!40000 ALTER TABLE `assay_source` DISABLE KEYS */;
/*!40000 ALTER TABLE `assay_source` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `chemical`
--

DROP TABLE IF EXISTS `chemical`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `chemical` (
  `chid` int(11) NOT NULL AUTO_INCREMENT,
  `casn` varchar(45) DEFAULT NULL,
  `chnm` varchar(255) NOT NULL,
  PRIMARY KEY (`chid`),
  UNIQUE KEY `casn` (`casn`),
  UNIQUE KEY `chnm` (`chnm`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `chemical`
--

LOCK TABLES `chemical` WRITE;
/*!40000 ALTER TABLE `chemical` DISABLE KEYS */;
/*!40000 ALTER TABLE `chemical` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `chemical_library`
--

DROP TABLE IF EXISTS `chemical_library`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `chemical_library` (
  `chid` int(11) NOT NULL,
  `clib` varchar(30) NOT NULL,
  PRIMARY KEY (`chid`,`clib`),
  KEY `chid` (`chid`) USING BTREE,
  KEY `clib` (`clib`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `chemical_library`
--

LOCK TABLES `chemical_library` WRITE;
/*!40000 ALTER TABLE `chemical_library` DISABLE KEYS */;
/*!40000 ALTER TABLE `chemical_library` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `citations`
--

DROP TABLE IF EXISTS `citations`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `citations` (
  `citation_id` int(11) NOT NULL,
  `pmid` int(11) DEFAULT NULL,
  `doi` mediumtext COLLATE latin1_general_ci,
  `other_source` mediumtext COLLATE latin1_general_ci,
  `other_id` mediumtext COLLATE latin1_general_ci,
  `citation` longtext COLLATE latin1_general_ci NOT NULL,
  `title` longtext COLLATE latin1_general_ci,
  `author` longtext COLLATE latin1_general_ci,
  `url` longtext COLLATE latin1_general_ci,
  PRIMARY KEY (`citation_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `citations`
--

LOCK TABLES `citations` WRITE;
/*!40000 ALTER TABLE `citations` DISABLE KEYS */;
/*!40000 ALTER TABLE `citations` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `gene`
--

DROP TABLE IF EXISTS `gene`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gene` (
  `gene_id` int(25) NOT NULL AUTO_INCREMENT,
  `entrez_gene_id` int(50) DEFAULT NULL,
  `official_full_name` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `gene_name` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `official_symbol` varchar(32) COLLATE latin1_general_ci DEFAULT NULL,
  `gene_symbol` varchar(32) COLLATE latin1_general_ci DEFAULT NULL,
  `description` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `uniprot_accession_number` varchar(32) COLLATE latin1_general_ci DEFAULT NULL,
  `organism_id` int(11) DEFAULT NULL,
  `track_status` varchar(32) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`gene_id`),
  UNIQUE KEY `entrez_gene_id` (`entrez_gene_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `gene`
--

LOCK TABLES `gene` WRITE;
/*!40000 ALTER TABLE `gene` DISABLE KEYS */;
/*!40000 ALTER TABLE `gene` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `intended_target`
--

DROP TABLE IF EXISTS `intended_target`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `intended_target` (
  `aeid` int(11) DEFAULT NULL,
  `target_id` int(11) DEFAULT NULL,
  `source` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  KEY `aeid_idx` (`aeid`),
  KEY `target_id_idx` (`target_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `intended_target`
--

LOCK TABLES `intended_target` WRITE;
/*!40000 ALTER TABLE `intended_target` DISABLE KEYS */;
/*!40000 ALTER TABLE `intended_target` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc0`
--

DROP TABLE IF EXISTS `mc0`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc0` (
  `m0id` bigint(20) NOT NULL AUTO_INCREMENT,
  `acid` bigint(20) unsigned NOT NULL DEFAULT '0',
  `spid` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `apid` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  `rowi` int(11) DEFAULT NULL,
  `coli` int(11) DEFAULT NULL,
  `wllt` varchar(1) COLLATE latin1_general_ci NOT NULL,
  `wllq` smallint(1) NOT NULL,
  `conc` double DEFAULT NULL,
  `rval` double DEFAULT NULL,
  `srcf` varchar(255) COLLATE latin1_general_ci NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`m0id`),
  KEY `wllt` (`wllt`) USING BTREE,
  KEY `acid` (`acid`),
  KEY `spid` (`spid`) USING BTREE,
  KEY `SPID/ACID` (`acid`,`spid`),
  KEY `srcf` (`srcf`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc0`
--

LOCK TABLES `mc0` WRITE;
/*!40000 ALTER TABLE `mc0` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc0` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc1`
--

DROP TABLE IF EXISTS `mc1`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc1` (
  `m1id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `m0id` bigint(20) unsigned DEFAULT NULL,
  `acid` bigint(20) unsigned DEFAULT NULL,
  `cndx` int(11) DEFAULT NULL,
  `repi` int(11) DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`m1id`),
  UNIQUE KEY `m1id` (`m1id`),
  KEY `acid` (`acid`) USING BTREE,
  KEY `m0id` (`m0id`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc1`
--

LOCK TABLES `mc1` WRITE;
/*!40000 ALTER TABLE `mc1` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc1` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc2`
--

DROP TABLE IF EXISTS `mc2`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc2` (
  `m2id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `m0id` bigint(20) unsigned DEFAULT NULL,
  `acid` bigint(20) unsigned DEFAULT NULL,
  `m1id` bigint(20) unsigned DEFAULT NULL,
  `cval` double NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`m2id`),
  UNIQUE KEY `m2id` (`m2id`),
  KEY `acid` (`acid`) USING BTREE,
  KEY `m0id` (`m0id`) USING BTREE,
  KEY `m1id` (`m1id`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc2`
--

LOCK TABLES `mc2` WRITE;
/*!40000 ALTER TABLE `mc2` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc2` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc2_acid`
--

DROP TABLE IF EXISTS `mc2_acid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc2_acid` (
  `mc2_mthd_id` int(11) NOT NULL,
  `acid` int(11) NOT NULL,
  `exec_ordr` int(11) NOT NULL DEFAULT '1',
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`mc2_mthd_id`,`acid`),
  UNIQUE KEY `mc2_acid_unique_ordr` (`acid`, `exec_ordr`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc2_acid`
--

LOCK TABLES `mc2_acid` WRITE;
/*!40000 ALTER TABLE `mc2_acid` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc2_acid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc2_methods`
--

DROP TABLE IF EXISTS `mc2_methods`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc2_methods` (
  `mc2_mthd_id` int(11) NOT NULL AUTO_INCREMENT,
  `mc2_mthd` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `desc` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`mc2_mthd_id`),
  UNIQUE KEY `mc2_method` (`mc2_mthd`)
) ENGINE=MyISAM AUTO_INCREMENT=18 DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc2_methods`
--

LOCK TABLES `mc2_methods` WRITE;
/*!40000 ALTER TABLE `mc2_methods` DISABLE KEYS */;
INSERT INTO `mc2_methods` VALUES (1,'none','apply no level 2 method','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(2,'log2','log2 all raw data','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(3,'rmneg','remove negative values prior to logging values','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(4,'rmzero','remove 0 values prior to logging values','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(5,'mult25','multiply values by 25','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(7,'mult100','multiply values by 100','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(10,'log10','log10 the raw data','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(11,'negshift','Shifts all data to be positive (new min=1)','2014-10-06 18:47:15','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02'),(12,'mult2.5','multiply values by 2.5','2014-11-06 15:36:15','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02'),(13,'mult3','multiply values by 3','2014-11-06 15:36:34','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02'),(14,'mult6','multiply values by 6','2014-11-06 20:53:33','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02'),(17,'sub100','100 minus cval (centers data around zero if prenormalized starting at 100 and going down to zero)','2015-10-05 16:42:21','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02\r\n');
/*!40000 ALTER TABLE `mc2_methods` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc3`
--

DROP TABLE IF EXISTS `mc3`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc3` (
  `m3id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `aeid` bigint(20) unsigned DEFAULT NULL,
  `m0id` bigint(20) unsigned DEFAULT NULL,
  `acid` bigint(20) unsigned DEFAULT NULL,
  `m1id` bigint(20) unsigned DEFAULT NULL,
  `m2id` bigint(20) unsigned DEFAULT NULL,
  `bval` double DEFAULT NULL,
  `pval` double DEFAULT NULL,
  `logc` double DEFAULT NULL,
  `resp` double NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`m3id`),
  UNIQUE KEY `m3id` (`m3id`),
  KEY `aeid` (`aeid`) USING BTREE,
  KEY `acid` (`acid`) USING BTREE,
  KEY `m0id` (`m0id`) USING BTREE,
  KEY `m1id` (`m1id`) USING BTREE,
  KEY `m2id` (`m2id`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc3`
--

LOCK TABLES `mc3` WRITE;
/*!40000 ALTER TABLE `mc3` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc3` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc3_aeid`
--

DROP TABLE IF EXISTS `mc3_aeid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc3_aeid` (
  `mc3_mthd_id` int(11) NOT NULL,
  `aeid` int(11) NOT NULL,
  `exec_ordr` int(11) NOT NULL DEFAULT '1',
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`mc3_mthd_id`,`aeid`),
  UNIQUE KEY `mc3_aeid_unique_ordr` (`aeid`, `exec_ordr`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc3_aeid`
--

LOCK TABLES `mc3_aeid` WRITE;
/*!40000 ALTER TABLE `mc3_aeid` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc3_aeid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc3_methods`
--

DROP TABLE IF EXISTS `mc3_methods`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc3_methods` (
  `mc3_mthd_id` int(11) NOT NULL AUTO_INCREMENT,
  `mc3_mthd` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `desc` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`mc3_mthd_id`),
  UNIQUE KEY `mc3_method` (`mc3_mthd`)
) ENGINE=MyISAM AUTO_INCREMENT=39 DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc3_methods`
--

LOCK TABLES `mc3_methods` WRITE;
/*!40000 ALTER TABLE `mc3_methods` DISABLE KEYS */;
INSERT INTO `mc3_methods` VALUES (1,'none','apply no level 3 method','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(2,'bval.apid.lowconc.med','plate-wise baseline based on low conc median value','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(3,'pval.apid.medpcbyconc.max','plate-wise median response of positive control (max)','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(4,'pval.apid.medpcbyconc.min','plate-wise median response of positive control (min)','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(5,'resp.pc','response percent activity','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(6,'resp.multneg1','multiply the response by -1','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(7,'resp.log2','take the log base 2 of the response','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(8,'resp.mult25','multiply the response by 25','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(9,'resp.fc','calculate response as fold-change','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(11,'bval.apid.nwlls.med','plate-wise baseline based on neutral ctrl median value','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(12,'bval.spid.lowconc.med','sample-wise baseline based on low conc median value','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(13,'pval.apid.pwlls.med','plate-wise meidan based on positive control, single dose','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(14,'pval.apid.mwlls.med','plate-wise meidan based on negative control, single dose','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(15,'pval.apid.medncbyconc.min','plate-wise meidan based on negative control, (min)','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(16,'bval.apid.twlls.med','Take the median cval of the t wells, by apid','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(17,'bval.apid.nwllslowconc.med','Take the median cval of the n wells and the first two concentrations, by apid','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(18,'resp.shiftneg.3bmad','Make values below baseline zero.','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(19,'resp.blineshift.3bmad.repi','Do baseline correction by repi, with a window of 3*bmad','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(20,'resp.blineshift.50.repi','Do baseline correction by repi, with a window of 50','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(21,'resp.blineshift.50.spid','Do baseline correction by spid, with a window of 50','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(23,'resp.blineshift.3bmad.spid','Do baseline correction by repi, with a window of 3*bmad','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(24,'bval.apid.tn.med','Take the median cval of the t and n wells, by apid','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(25,'pval.apid.pmv.min','Calculate the median p, m, and v values by concentration, then take the minimum by apid.','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(26,'pval.apid.pmv.max','Calculate the median p, m, and v values by concentration, then take the maximum by apid.','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(27,'pval.apid.f.max','Calculate the median of f values by concentration, then take the maximum by apid','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(28,'pval.apid.f.min','Calculate the median of f values by concentration, then take the minimum by apid','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(29,'pval.apid.p.min','Calculate the median of p values by concentration, then take the minimum by apid','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(30,'pval.apid.p.max','Calculate the median of p values by concentration, then take the maximum by apid','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(31,'pval.apid.v.min','Calculate the median of v values by concentration, then take the minimum by apid','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(32,'pval.zero','Set pval to 0.','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(33,'resp.shiftneg.6bmad','Shift response values falling below -6 * bmad to 0.','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(34,'resp.shiftneg.10bmad','Shift response values falling below -10 * bmad to 0.','0000-00-00 00:00:00','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(35,'resp.logfc','Calculate the response as a fold change over baseline for logged values','2014-10-02 18:18:40','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(36,'resp.scale.mad.log2fc','Scale the resp data by the ratio of log2(1.2) over 3* baseline mad of the unscaled resp values','2015-02-23 18:58:52','0000-00-00 00:00:00',''),(37,'resp.scale.quant.log2fc','Scale resp such that 20% of the maximum resp equals log2(1.2)','2015-03-13 20:44:05','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(38,'bval.apid.nwllstcwllslowconc.med','plate-wise baseline based on neutral and t and c well low conc','2015-09-16 19:22:54','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02');
/*!40000 ALTER TABLE `mc3_methods` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc4`
--

DROP TABLE IF EXISTS `mc4`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc4` (
  `m4id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `aeid` bigint(20) unsigned NOT NULL,
  `spid` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `bmad` double NOT NULL,
  `resp_max` double NOT NULL,
  `resp_min` double NOT NULL,
  `max_mean` double NOT NULL,
  `max_mean_conc` double NOT NULL,
  `max_med` double NOT NULL,
  `max_med_conc` double NOT NULL,
  `logc_max` double NOT NULL,
  `logc_min` double NOT NULL,
  `cnst` tinyint(1) DEFAULT NULL,
  `hill` tinyint(1) DEFAULT NULL,
  `hcov` tinyint(1) DEFAULT NULL,
  `gnls` tinyint(1) DEFAULT NULL,
  `gcov` tinyint(1) DEFAULT NULL,
  `cnst_er` double DEFAULT NULL,
  `cnst_aic` double DEFAULT NULL,
  `cnst_rmse` double DEFAULT NULL,
  `cnst_prob` double DEFAULT NULL,
  `hill_tp` double DEFAULT NULL,
  `hill_tp_sd` double DEFAULT NULL,
  `hill_ga` double DEFAULT NULL,
  `hill_ga_sd` double DEFAULT NULL,
  `hill_gw` double DEFAULT NULL,
  `hill_gw_sd` double DEFAULT NULL,
  `hill_er` double DEFAULT NULL,
  `hill_er_sd` double DEFAULT NULL,
  `hill_aic` double DEFAULT NULL,
  `hill_rmse` double DEFAULT NULL,
  `hill_prob` double DEFAULT NULL,
  `gnls_tp` double DEFAULT NULL,
  `gnls_tp_sd` double DEFAULT NULL,
  `gnls_ga` double DEFAULT NULL,
  `gnls_ga_sd` double DEFAULT NULL,
  `gnls_gw` double DEFAULT NULL,
  `gnls_gw_sd` double DEFAULT NULL,
  `gnls_la` double DEFAULT NULL,
  `gnls_la_sd` double DEFAULT NULL,
  `gnls_lw` double DEFAULT NULL,
  `gnls_lw_sd` double DEFAULT NULL,
  `gnls_er` double DEFAULT NULL,
  `gnls_er_sd` double DEFAULT NULL,
  `gnls_aic` double DEFAULT NULL,
  `gnls_rmse` double DEFAULT NULL,
  `gnls_prob` double DEFAULT NULL,
  `nconc` int(11) NOT NULL,
  `npts` int(11) NOT NULL,
  `nrep` double NOT NULL,
  `nmed_gtbl` int(11) NOT NULL,
  `tmpi` int(11) NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`m4id`),
  UNIQUE KEY `m4id` (`m4id`),
  KEY `aeid` (`aeid`) USING BTREE,
  KEY `spid` (`spid`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc4`
--

LOCK TABLES `mc4` WRITE;
/*!40000 ALTER TABLE `mc4` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc4` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc4_agg`
--

DROP TABLE IF EXISTS `mc4_agg`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc4_agg` (
  `aeid` bigint(20) unsigned NOT NULL,
  `m0id` bigint(20) unsigned NOT NULL,
  `m1id` bigint(20) unsigned NOT NULL,
  `m2id` bigint(20) unsigned NOT NULL,
  `m3id` bigint(20) unsigned NOT NULL,
  `m4id` bigint(20) unsigned NOT NULL,
  KEY `aeid` (`aeid`) USING BTREE,
  KEY `m4id` (`m4id`) USING BTREE,
  KEY `m3id` (`m3id`) USING BTREE,
  KEY `m2id` (`m2id`) USING BTREE,
  KEY `m1id` (`m1id`) USING BTREE,
  KEY `m0id` (`m0id`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc4_agg`
--

LOCK TABLES `mc4_agg` WRITE;
/*!40000 ALTER TABLE `mc4_agg` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc4_agg` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc5`
--

DROP TABLE IF EXISTS `mc5`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc5` (
  `m5id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `m4id` bigint(20) unsigned NOT NULL,
  `aeid` bigint(20) unsigned NOT NULL,
  `modl` varchar(4) COLLATE latin1_general_ci DEFAULT 'cnst',
  `hitc` tinyint(4) DEFAULT '0',
  `fitc` tinyint(3) unsigned DEFAULT '1',
  `coff` double DEFAULT NULL,
  `actp` double DEFAULT NULL,
  `modl_er` double DEFAULT NULL,
  `modl_tp` double DEFAULT NULL,
  `modl_ga` double DEFAULT NULL,
  `modl_gw` double DEFAULT NULL,
  `modl_la` double DEFAULT NULL,
  `modl_lw` double DEFAULT NULL,
  `modl_prob` double DEFAULT NULL,
  `modl_rmse` double DEFAULT NULL,
  `modl_acc` double DEFAULT NULL,
  `modl_acb` double DEFAULT NULL,
  `modl_ac10` double DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`m5id`),
  UNIQUE KEY `m5id` (`m5id`),
  KEY `aeid` (`aeid`) USING BTREE,
  KEY `fitc` (`fitc`) USING BTREE,
  KEY `m4id` (`m4id`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc5`
--

LOCK TABLES `mc5` WRITE;
/*!40000 ALTER TABLE `mc5` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc5` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc5_aeid`
--

DROP TABLE IF EXISTS `mc5_aeid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc5_aeid` (
  `aeid` bigint(20) unsigned NOT NULL,
  `mc5_mthd_id` int(10) unsigned NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`mc5_mthd_id`,`aeid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc5_aeid`
--

LOCK TABLES `mc5_aeid` WRITE;
/*!40000 ALTER TABLE `mc5_aeid` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc5_aeid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc5_fit_categories`
--

DROP TABLE IF EXISTS `mc5_fit_categories`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc5_fit_categories` (
  `fitc` int(11) NOT NULL,
  `parent_fitc` int(11) DEFAULT NULL,
  `name` varchar(30) COLLATE latin1_general_ci NOT NULL,
  `xloc` double NOT NULL,
  `yloc` double NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc5_fit_categories`
--

LOCK TABLES `mc5_fit_categories` WRITE;
/*!40000 ALTER TABLE `mc5_fit_categories` DISABLE KEYS */;
INSERT INTO `mc5_fit_categories` VALUES (1,NULL,'01: ALL DOSE RESPONSE',1110,756.5),(2,1,'02: CANNOT DETERMINE',977.7455523765,611.9831921304),(3,1,'03: INACTIVE',1022.9653320313,924.2873535156),(4,3,'04: RESP < BLINE',1271.6083984375,974.8182373047),(5,3,'05: RESP >= BLINE',996.8209228516,1127.5755004883),(6,5,'06: CNST',666,1251.5),(7,6,'07: NOTP >= 0.8(COFF)',761.8584594727,1333.5216674805),(8,6,'08: ANYTP >= 0.8(COFF)',507.3630981445,1333.6228027344),(9,8,'09: NOTP >= COFF',337.4776000977,1263.4637451172),(10,8,'10: ANYTP >= COFF',450.4357910156,1397.3994140625),(11,5,'11: HILL',1189.7768554688,1209.4105834961),(12,11,'12: TP < 0.8(COFF)',1149.2390136719,1317.5518188477),(13,12,'13: GNLS < 0.8(COFF)',992.1193237305,1394.7369384766),(14,12,'14: GNLS >= 0.8(COFF)',1246.0364990234,1386.9293518066),(15,14,'15: GNLS < COFF',1218.9203491211,1496.1217041016),(16,14,'16: GNLS >= COFF',1361.9873046875,1443.5811157227),(17,11,'17: TP >= 0.8(COFF)',1460.6746826172,1119.6658935547),(18,17,'18: GNLS < 0.8(COFF)',1539.3952956084,1228.8681030273),(19,17,'19: GNLS >= 0.8(COFF)',1623.3784179688,1043.5646362305),(20,19,'20: GNLS < COFF',1876.1442871094,1040.0154418945),(21,19,'21: GNLS >= COFF',1849.7615966797,948.6744995117),(22,5,'22: GNLS',794.5350952148,1043.2316894531),(23,22,'23: TP < 0.8(COFF)',632.9550857544,936.3232232708),(24,23,'24: HILL < 0.8(COFF)',803.6691931111,753.821326896),(25,23,'25: HILL >= 0.8(COFF)',542.0636749268,831.3344537395),(26,25,'26: HILL < COFF',478.504219532,698.9990045207),(27,25,'27: HILL >= COFF',359.5413589478,763.8884088176),(28,22,'28: TP >= 0.8(COFF)',491.8406066895,1063.1170043945),(29,28,'29: HILL < 0.8(COFF)',446.9388122559,1132.0888671875),(30,28,'30: HILL >= 0.8(COFF)',371.3406066895,956.1169433594),(31,30,'31: HILL < COFF',288.3405761719,853.1170654297),(32,30,'32: HILL >= COFF',107.8862915039,948.5189208984),(33,1,'33: ACTIVE',1276.5,563.5),(34,33,'34: HILL',1190.094543457,462.0799560547),(35,34,'35: TP <= 1.2(COFF)',991.5,302.5),(36,35,'36: AC50 <=',764.6481933594,350.3658447266),(37,35,'37: AC50 ==',749.6301269531,254.4168701172),(38,35,'38: AC50 >=',966.6704101563,183.7718505859),(39,34,'39: TP > 1.2(COFF)',1310.6741943359,263.3843994141),(40,39,'40: AC50 <=',1134.5294799805,105.1185302734),(41,39,'41: AC50 ==',1412.8084716797,121.1416015625),(42,39,'42: AC50 >=',1578.5758056641,187.1433105469),(43,33,'43: GNLS',1558.3104248047,570.28125),(44,43,'44: TP <= 1.2(COFF)',1639.5,451.5),(45,44,'45: AC50 <=',1811.0447998047,377.1083984375),(46,44,'46: AC50 ==',1905.2620849609,445.5483398437),(47,44,'47: AC50 >=',1659.5,320.5),(48,43,'48: TP > 1.2(COFF)',1647.5,685.5),(49,48,'49: AC50 <=',1704.5,806.5),(50,48,'50: AC50 ==',1873.9647216797,741.5611572266),(51,48,'51: AC50 >=',1926.5576171875,671.6712646484),(52,23,'52: HILL DNC',890.4145965576,837.0693170207),(53,28,'53: HILL DNC',155.8844682354,1077.1187744141),(54,12,'54: GNLS DNC',1463.7992844698,1325.916809082),(55,17,'55: GNLS DNC',1774.73046875,1155.2709031221);
/*!40000 ALTER TABLE `mc5_fit_categories` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc5_methods`
--

DROP TABLE IF EXISTS `mc5_methods`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc5_methods` (
  `mc5_mthd_id` int(11) NOT NULL AUTO_INCREMENT,
  `mc5_mthd` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `desc` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`mc5_mthd_id`),
  UNIQUE KEY `mc5_methods` (`mc5_mthd`)
) ENGINE=MyISAM AUTO_INCREMENT=13 DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc5_methods`
--

LOCK TABLES `mc5_methods` WRITE;
/*!40000 ALTER TABLE `mc5_methods` DISABLE KEYS */;
INSERT INTO `mc5_methods` VALUES (1,'bmad3','Add a cutoff value of 3*bmad.','2015-05-04 16:03:49','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(2,'pc20','Add a cutoff value of 20.','2015-05-04 16:03:49','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(3,'log2_1.2','Add a cutoff value of log2(1.2).','2015-05-04 16:03:49','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(4,'log10_1.2','Add a cutoff value of log10(1.2).','2015-05-04 16:03:49','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(5,'bmad5','Add a cutoff value of 5*bmad.','2015-05-04 16:03:49','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(6,'bmad6','Add a cutoff value of 6*bmad.','2015-05-04 16:03:49','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(7,'bmad10','Add a cutoff value of 10*bmad.','2015-05-04 16:03:49','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(8,'maxmed20pct','Add a cutoff value of 20% of the max of max_med','2015-05-06 13:08:50','0000-00-00 00:00:00','kconnors.kconnors.kconnors'),(9,'pc70','70% cutoff','2015-07-28 19:06:35','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02'),(10,'pc50','50% cutoff','2015-07-28 19:08:01','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02'),(11,'log2_2','log2 of 2 fold change cutoff','2015-07-28 19:08:01','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02'),(12,'log10_2','log10 of 2 fold change cutoff','2015-07-28 19:08:01','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02');
/*!40000 ALTER TABLE `mc5_methods` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc6`
--

DROP TABLE IF EXISTS `mc6`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc6` (
  `m6id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `m5id` bigint(20) unsigned NOT NULL,
  `m4id` bigint(20) unsigned NOT NULL,
  `aeid` bigint(20) unsigned NOT NULL,
  `mc6_mthd_id` bigint(20) unsigned NOT NULL,
  `flag` varchar(255) COLLATE latin1_general_ci NOT NULL,
  `fval` double DEFAULT NULL,
  `fval_unit` varchar(45) COLLATE latin1_general_ci DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`m6id`),
  UNIQUE KEY `m6id` (`m6id`),
  KEY `aeid` (`aeid`) USING BTREE,
  KEY `m4id` (`m4id`) USING BTREE,
  KEY `m5id` (`m5id`) USING BTREE,
  KEY `mc6_mthd_id` (`mc6_mthd_id`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc6`
--

LOCK TABLES `mc6` WRITE;
/*!40000 ALTER TABLE `mc6` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc6` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc6_aeid`
--

DROP TABLE IF EXISTS `mc6_aeid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc6_aeid` (
  `aeid` bigint(20) unsigned NOT NULL,
  `mc6_mthd_id` int(10) unsigned NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`mc6_mthd_id`,`aeid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc6_aeid`
--

LOCK TABLES `mc6_aeid` WRITE;
/*!40000 ALTER TABLE `mc6_aeid` DISABLE KEYS */;
/*!40000 ALTER TABLE `mc6_aeid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mc6_methods`
--

DROP TABLE IF EXISTS `mc6_methods`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mc6_methods` (
  `mc6_mthd_id` int(11) NOT NULL AUTO_INCREMENT,
  `mc6_mthd` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `desc` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `nddr` tinyint(1) NOT NULL DEFAULT '0',
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`mc6_mthd_id`),
  UNIQUE KEY `mc6_method` (`mc6_mthd`)
) ENGINE=MyISAM AUTO_INCREMENT=18 DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mc6_methods`
--

LOCK TABLES `mc6_methods` WRITE;
/*!40000 ALTER TABLE `mc6_methods` DISABLE KEYS */;
INSERT INTO `mc6_methods` VALUES (1,'row.dev.up','Look for row-wise plate effects, increase',1,'2014-07-28 18:03:41','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(2,'row.dev.dn','Look for row-wise plate effects, decrease',1,'2014-07-28 18:03:54','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(3,'col.dev.dn','Look for column-wise plate effects, decrease',1,'2014-07-28 18:04:08','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(4,'col.dev.up','Look for column-wise plate effects, increase',1,'2014-07-28 18:04:17','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(5,'plate.flare','Look for plate flare effects',1,'2014-07-28 18:04:27','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(6,'singlept.hit.high','Look for single point hits with activity only at the highest conc tested',0,'2014-07-28 18:06:05','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(7,'singlept.hit.mid','Look for signle point hits with activity not at highest conc tested',0,'2014-07-28 18:06:51','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(8,'multipoint.neg','Look for inactives with multiple medians above baseline',0,'2014-07-28 18:08:42','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(9,'pintool','Look for pintool carryover issues',1,'2014-07-28 18:09:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(10,'noise','Look for noisy curves, relative to the assay',0,'2014-07-28 18:09:41','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(11,'border.hit','Look for actives with borderline activity',0,'2014-07-28 18:10:14','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(12,'border.miss','Look for inactives with borderline activity',0,'2014-07-28 18:10:21','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(13,'plate.interlace','Look for interlaced chemical-plate effects',1,'2014-08-21 16:13:26','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(14,'rep.mismatch','Look for mismatched techinal replicates',1,'2014-08-21 16:16:44','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(15,'gnls.lowconc','Look for low concentration gnls winners',0,'2014-08-21 16:21:32','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(16,'overfit.hit','Flag hit-calls that would get changed after doing the small N correction to the aic values.',0,'2014-09-16 15:51:14','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(17,'efficacy.50','Flag hit-calls with efficacy values less than 50% -- intended for biochemical assays.',0,'2014-09-16 15:51:58','0000-00-00 00:00:00','dfiler.dfiler.dfiler');
/*!40000 ALTER TABLE `mc6_methods` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `organism`
--

DROP TABLE IF EXISTS `organism`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `organism` (
  `organism_id` int(11) NOT NULL AUTO_INCREMENT,
  `ncbi_taxon_id` int(11) DEFAULT NULL,
  `taxon_name` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `common_name` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `lineage` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`organism_id`),
  UNIQUE KEY `ncbi_taxon_id` (`ncbi_taxon_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `organism`
--

LOCK TABLES `organism` WRITE;
/*!40000 ALTER TABLE `organism` DISABLE KEYS */;
/*!40000 ALTER TABLE `organism` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sample`
--

DROP TABLE IF EXISTS `sample`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sample` (
  `spid` varchar(50) NOT NULL,
  `chid` int(11) DEFAULT NULL,
  `stkc` double DEFAULT NULL,
  `stkc_unit` varchar(50) DEFAULT NULL,
  `tested_conc_unit` varchar(50) DEFAULT NULL,
  `spid_legacy` varchar(50) DEFAULT NULL,
  PRIMARY KEY (`spid`),
  KEY `chid` (`chid`) USING BTREE,
  KEY `spid_legacy` (`spid_legacy`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sample`
--

LOCK TABLES `sample` WRITE;
/*!40000 ALTER TABLE `sample` DISABLE KEYS */;
/*!40000 ALTER TABLE `sample` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sc0`
--

DROP TABLE IF EXISTS `sc0`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sc0` (
  `s0id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `acid` bigint(20) unsigned NOT NULL,
  `spid` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `apid` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  `rowi` int(11) DEFAULT NULL,
  `coli` int(11) DEFAULT NULL,
  `wllt` varchar(1) COLLATE latin1_general_ci NOT NULL,
  `wllq` smallint(1) NOT NULL,
  `conc` double DEFAULT NULL,
  `rval` double DEFAULT NULL,
  `srcf` varchar(255) COLLATE latin1_general_ci NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`s0id`),
  UNIQUE KEY `s0id` (`s0id`),
  KEY `acid` (`acid`) USING BTREE,
  KEY `wllt` (`wllt`) USING BTREE,
  KEY `spid` (`spid`) USING BTREE,
  KEY `srcf` (`srcf`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sc0`
--

LOCK TABLES `sc0` WRITE;
/*!40000 ALTER TABLE `sc0` DISABLE KEYS */;
/*!40000 ALTER TABLE `sc0` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sc1`
--

DROP TABLE IF EXISTS `sc1`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sc1` (
  `s1id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `s0id` int(11) NOT NULL,
  `acid` int(11) NOT NULL,
  `aeid` int(11) NOT NULL,
  `logc` double DEFAULT NULL,
  `bval` double DEFAULT NULL,
  `pval` double DEFAULT NULL,
  `resp` double NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(250) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`s1id`),
  UNIQUE KEY `s1id` (`s1id`),
  KEY `s0id` (`s0id`),
  KEY `acid` (`acid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sc1`
--

LOCK TABLES `sc1` WRITE;
/*!40000 ALTER TABLE `sc1` DISABLE KEYS */;
/*!40000 ALTER TABLE `sc1` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sc1_aeid`
--

DROP TABLE IF EXISTS `sc1_aeid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sc1_aeid` (
  `sc1_mthd_id` int(11) NOT NULL,
  `aeid` int(11) NOT NULL,
  `exec_ordr` int(11) NOT NULL DEFAULT '1',
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`sc1_mthd_id`,`aeid`),
  UNIQUE KEY `sc1_aeid_unique_ordr` (`aeid`, `exec_ordr`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sc1_aeid`
--

LOCK TABLES `sc1_aeid` WRITE;
/*!40000 ALTER TABLE `sc1_aeid` DISABLE KEYS */;
/*!40000 ALTER TABLE `sc1_aeid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sc1_methods`
--

DROP TABLE IF EXISTS `sc1_methods`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sc1_methods` (
  `sc1_mthd_id` int(11) NOT NULL AUTO_INCREMENT,
  `sc1_mthd` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `desc` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`sc1_mthd_id`),
  UNIQUE KEY `sc1_method` (`sc1_mthd`)
) ENGINE=MyISAM AUTO_INCREMENT=18 DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sc1_methods`
--

LOCK TABLES `sc1_methods` WRITE;
/*!40000 ALTER TABLE `sc1_methods` DISABLE KEYS */;
INSERT INTO `sc1_methods` VALUES (1,'bval.apid.nwlls.med','plate-wise baseline based on neutral ctrl median value','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(2,'bval.apid.twlls.med','Take the median cval of the t wells, by apid','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(3,'bval.apid.tn.med','Take the median cval of the t and n wells, by apid','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(4,'pval.apid.pwlls.med','plate-wise meidan based on positive control, single dose','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(5,'pval.apid.mwlls.med','plate-wise meidan based on negative control, single dose','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(6,'pval.apid.medpcbyconc.max','plate-wise median response of positive control (max)','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(7,'pval.apid.medpcbyconc.min','plate-wise median response of positive control (min)','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(8,'pval.apid.medncbyconc.min','plate-wise meidan based on negative control, (min)','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(9,'pval.zero','Set pval to 0.','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(10,'resp.pc','response percent activity','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(11,'resp.fc','calculate response as fold-change','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(12,'resp.logfc','Calculate the response as a fold change over baseline for logged values','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(13,'resp.log2','take the log base 2 of the response','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(14,'none','apply no level 2 method','2015-05-13 15:44:35','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(16,'resp.multneg1','multiply response values by negative 1','2015-05-29 14:27:45','0000-00-00 00:00:00','Akarmaus.Akarmaus.Akarmaus'),(17,'pval.apid.or.aeid.pwlls.med','calculates pval first based on p wells by plate and then for any plates missing p wells it calculates a pval by median of pval for all other plates','2015-09-30 18:43:40','0000-00-00 00:00:00','mmarti02.mmarti02.mmarti02');
/*!40000 ALTER TABLE `sc1_methods` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sc2`
--

DROP TABLE IF EXISTS `sc2`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sc2` (
  `s2id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `aeid` int(11) NOT NULL,
  `spid` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `bmad` double NOT NULL,
  `max_med` double NOT NULL,
  `coff` double NOT NULL,
  `hitc` double NOT NULL,
  `tmpi` int(11) NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `created_by` varchar(250) COLLATE latin1_general_ci NOT NULL DEFAULT 'System',
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(250) COLLATE latin1_general_ci DEFAULT NULL,
  PRIMARY KEY (`s2id`),
  UNIQUE KEY `s2id` (`s2id`),
  KEY `acid` (`aeid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sc2`
--

LOCK TABLES `sc2` WRITE;
/*!40000 ALTER TABLE `sc2` DISABLE KEYS */;
/*!40000 ALTER TABLE `sc2` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sc2_aeid`
--

DROP TABLE IF EXISTS `sc2_aeid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sc2_aeid` (
  `sc2_mthd_id` int(11) NOT NULL,
  `aeid` int(11) NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`sc2_mthd_id`,`aeid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sc2_aeid`
--

LOCK TABLES `sc2_aeid` WRITE;
/*!40000 ALTER TABLE `sc2_aeid` DISABLE KEYS */;
/*!40000 ALTER TABLE `sc2_aeid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sc2_agg`
--

DROP TABLE IF EXISTS `sc2_agg`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sc2_agg` (
  `aeid` bigint(20) unsigned NOT NULL,
  `s0id` bigint(20) unsigned NOT NULL,
  `s1id` bigint(20) unsigned NOT NULL,
  `s2id` bigint(20) unsigned NOT NULL,
  KEY `aeid` (`aeid`) USING BTREE,
  KEY `s2id` (`s2id`) USING BTREE,
  KEY `s1id` (`s1id`) USING BTREE,
  KEY `s0id` (`s0id`) USING BTREE
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sc2_agg`
--

LOCK TABLES `sc2_agg` WRITE;
/*!40000 ALTER TABLE `sc2_agg` DISABLE KEYS */;
/*!40000 ALTER TABLE `sc2_agg` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sc2_methods`
--

DROP TABLE IF EXISTS `sc2_methods`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sc2_methods` (
  `sc2_mthd_id` int(11) NOT NULL AUTO_INCREMENT,
  `sc2_mthd` varchar(50) COLLATE latin1_general_ci NOT NULL,
  `desc` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `modified_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified_by` varchar(100) COLLATE latin1_general_ci NOT NULL,
  PRIMARY KEY (`sc2_mthd_id`),
  UNIQUE KEY `sc2_method` (`sc2_mthd`)
) ENGINE=MyISAM AUTO_INCREMENT=11 DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sc2_methods`
--

LOCK TABLES `sc2_methods` WRITE;
/*!40000 ALTER TABLE `sc2_methods` DISABLE KEYS */;
INSERT INTO `sc2_methods` VALUES (1,'bmad3','Add a cutoff value of 3*bmad.','2015-05-13 17:51:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(2,'pc20','Add a cutoff value of 20.','2015-05-13 17:51:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(3,'log2_1.2','Add a cutoff value of log2(1.2).','2015-05-13 17:51:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(4,'log10_1.2','Add a cutoff value of log10(1.2).','2015-05-13 17:51:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(5,'bmad5','Add a cutoff value of 5*bmad.','2015-05-13 17:51:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(6,'bmad6','Add a cutoff value of 6*bmad.','2015-05-13 17:51:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(7,'bmad10','Add a cutoff value of 10*bmad.','2015-05-13 17:51:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(8,'pc30orbmad3','Add a cutoff value of either 30 or 3*bmad, whichever is less.','2015-05-13 17:51:13','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(9,'pc0.88','Add a cutoff value of 0.88.','2015-05-14 17:25:38','0000-00-00 00:00:00','dfiler.dfiler.dfiler'),(10,'log2_1.5','Add a cutoff value of log2(1.5).','2015-05-14 17:25:38','0000-00-00 00:00:00','dfiler.dfiler.dfiler');
/*!40000 ALTER TABLE `sc2_methods` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `technological_target`
--

DROP TABLE IF EXISTS `technological_target`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `technological_target` (
  `acid` int(11) DEFAULT NULL,
  `target_id` int(11) DEFAULT NULL,
  `source` varchar(255) COLLATE latin1_general_ci DEFAULT NULL,
  KEY `acid_idx` (`acid`),
  KEY `target_id_idx` (`target_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `technological_target`
--

LOCK TABLES `technological_target` WRITE;
/*!40000 ALTER TABLE `technological_target` DISABLE KEYS */;
/*!40000 ALTER TABLE `technological_target` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2015-10-15 13:51:27
