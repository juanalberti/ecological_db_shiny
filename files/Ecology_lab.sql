CREATE DATABASE Ecology_lab;
USE Ecology_lab;

SET FOREIGN_KEY_CHECKS=0;

-- ----------------------------
-- Table structure for ambiente
-- ----------------------------
DROP TABLE IF EXISTS `ambiente`;
CREATE TABLE `ambiente` (
  `id_ambiente` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_ambiente` varchar(45) NOT NULL,
  PRIMARY KEY (`id_ambiente`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of ambiente
-- ----------------------------

-- ----------------------------
-- Table structure for ciudad
-- ----------------------------
DROP TABLE IF EXISTS `ciudad`;
CREATE TABLE `ciudad` (
  `id` int(4) NOT NULL AUTO_INCREMENT,
  `ciudad_nombre` varchar(60) NOT NULL,
  `latitud` varchar(25) DEFAULT NULL,
  `longitud` varchar(25) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of ciudad
-- ----------------------------

-- ----------------------------
-- Table structure for dato
-- ----------------------------
DROP TABLE IF EXISTS `dato`;
CREATE TABLE `dato` (
  `id_dato` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_dato` varchar(45) NOT NULL,
  `fk_id_tipo_dato` int(11) NOT NULL,
  `json_mas_data` mediumtext,
  PRIMARY KEY (`id_dato`),
  KEY `fk_id_tipo_dato` (`fk_id_tipo_dato`),
  CONSTRAINT `fk_id_tipo_dato` FOREIGN KEY (`fk_id_tipo_dato`) REFERENCES `tipo_dato` (`id_tipo_dato`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of dato
-- ----------------------------

-- ----------------------------
-- Table structure for escala
-- ----------------------------
DROP TABLE IF EXISTS `escala`;
CREATE TABLE `escala` (
  `id_escala` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_escala` varchar(200) COLLATE utf8_spanish_ci DEFAULT NULL,
  `obs_escala` varchar(500) COLLATE utf8_spanish_ci DEFAULT NULL,
  `depende_de_escala` int(11) DEFAULT NULL,
  `latitud` varchar(30) COLLATE utf8_spanish_ci DEFAULT NULL,
  `longitud` varchar(30) COLLATE utf8_spanish_ci DEFAULT NULL,
  `fk_tipo_escala` int(11) DEFAULT NULL,
  `fk_id_experimento` int(11) DEFAULT NULL,
  PRIMARY KEY (`id_escala`),
  KEY `fk_tipo_escala1` (`fk_tipo_escala`),
  CONSTRAINT `fk_tipo_escala1` FOREIGN KEY (`fk_tipo_escala`) REFERENCES `tipo_escala` (`id_tipo_escala`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- ----------------------------
-- Records of escala
-- ----------------------------

-- ----------------------------
-- Table structure for experimento
-- ----------------------------
DROP TABLE IF EXISTS `experimento`;
CREATE TABLE `experimento` (
  `id_experimento` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_experimento` varchar(100) NOT NULL,
  `fecha_experimento` date NOT NULL,
  `anio` varchar(4) DEFAULT NULL,
  `fk_id_usuario` int(11) NOT NULL,
  `fk_id_tipo_experimento` int(11) NOT NULL,
  `obs_experimento` text NOT NULL,
  `escala_medicion` double(11,3) DEFAULT NULL,
  `unidad_medicion` varchar(10) DEFAULT NULL,
  `fk_id_ciudad` int(11) DEFAULT NULL,
  `fk_id_ambiente` int(11) DEFAULT NULL,
  PRIMARY KEY (`id_experimento`),
  KEY `fk_id_usuario` (`fk_id_usuario`),
  KEY `fk_id_tipo_experimento` (`fk_id_tipo_experimento`),
  CONSTRAINT `fk_id_tipo_experimento` FOREIGN KEY (`fk_id_tipo_experimento`) REFERENCES `tipo_experimento` (`id_tipo_experimento`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_usuario` FOREIGN KEY (`fk_id_usuario`) REFERENCES `usuarios` (`id_usuario`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of experimento
-- ----------------------------

-- ----------------------------
-- Table structure for factor
-- ----------------------------
DROP TABLE IF EXISTS `factor`;
CREATE TABLE `factor` (
  `id_factor` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_factor` varchar(45) NOT NULL,
  PRIMARY KEY (`id_factor`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of factor
-- ----------------------------

-- ----------------------------
-- Table structure for inserciones
-- ----------------------------
DROP TABLE IF EXISTS `inserciones`;
CREATE TABLE `inserciones` (
  `id_insert` int(11) NOT NULL AUTO_INCREMENT,
  `fecha_ins` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `fk_id_usuario` int(11) DEFAULT NULL,
  `fk_id_experimento` int(11) DEFAULT NULL,
  `hubo_update` int(1) DEFAULT NULL,
  PRIMARY KEY (`id_insert`),
  KEY `fk_id_usuario1_idx` (`fk_id_usuario`),
  KEY `fk_id_experimento_ins_idx` (`fk_id_experimento`),
  CONSTRAINT `fk_id_experimento_ins` FOREIGN KEY (`fk_id_experimento`) REFERENCES `experimento` (`id_experimento`) ON DELETE CASCADE ON UPDATE NO ACTION,
  CONSTRAINT `fk_id_usuario_ins` FOREIGN KEY (`fk_id_usuario`) REFERENCES `usuarios` (`id_usuario`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- ----------------------------
-- Records of inserciones
-- ----------------------------

-- ----------------------------
-- Table structure for nivel
-- ----------------------------
DROP TABLE IF EXISTS `nivel`;
CREATE TABLE `nivel` (
  `id_nivel` int(11) NOT NULL AUTO_INCREMENT,
  `fk_id_factor` int(11) NOT NULL,
  `nombre_nivel` varchar(45) NOT NULL,
  `obs_nivel` tinytext,
  PRIMARY KEY (`id_nivel`),
  KEY `fk_id_factor` (`fk_id_factor`),
  CONSTRAINT `fk_id_factor` FOREIGN KEY (`fk_id_factor`) REFERENCES `factor` (`id_factor`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of nivel
-- ----------------------------

-- ----------------------------
-- Table structure for registro
-- ----------------------------
DROP TABLE IF EXISTS `registro`;
CREATE TABLE `registro` (
  `id_registro` int(11) NOT NULL AUTO_INCREMENT,
  `fk_id_experimento` int(11) NOT NULL,
  `fk_id_ciudad` int(11) NOT NULL,
  `fk_id_ambiente` int(11) NOT NULL,
  `fk_id_usuario` int(11) NOT NULL,
  `fk_id_insercion` int(11) NOT NULL,
  `replica` int(11) DEFAULT NULL,
  `fecha_registro` date NOT NULL,
  `id_registro_padre` int(11) DEFAULT '0',
  `observacion_registro` varchar(255) DEFAULT NULL,
  `anio` varchar(4) DEFAULT NULL,
  `hora` varchar(4) DEFAULT NULL,
  `ref_hijo` varchar(8) DEFAULT NULL,
  `id_heading` varchar(60) NOT NULL,
  PRIMARY KEY (`id_registro`),
  KEY `fk_id_experimento1` (`fk_id_experimento`),
  KEY `fk_id_ciudad1` (`fk_id_ciudad`),
  KEY `fk_id_ambiente1` (`fk_id_ambiente`),
  KEY `fk_id_usuario1` (`fk_id_usuario`),
  KEY `fk_id_inserc_r_idx` (`fk_id_insercion`),
  CONSTRAINT `fk_id_ambiente1` FOREIGN KEY (`fk_id_ambiente`) REFERENCES `ambiente` (`id_ambiente`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_ciudad1` FOREIGN KEY (`fk_id_ciudad`) REFERENCES `ciudad` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_experimento1` FOREIGN KEY (`fk_id_experimento`) REFERENCES `experimento` (`id_experimento`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_insercion_reg` FOREIGN KEY (`fk_id_insercion`) REFERENCES `inserciones` (`id_insert`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_id_usuario1` FOREIGN KEY (`fk_id_usuario`) REFERENCES `usuarios` (`id_usuario`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of registro
-- ----------------------------

-- ----------------------------
-- Table structure for registro_escala
-- ----------------------------
DROP TABLE IF EXISTS `registro_escala`;
CREATE TABLE `registro_escala` (
  `id_registro_escala` int(11) NOT NULL AUTO_INCREMENT,
  `fk_id_registro` int(11) NOT NULL,
  `fk_id_escala` int(11) DEFAULT NULL,
  PRIMARY KEY (`id_registro_escala`),
  KEY `fk_id_escala3` (`fk_id_escala`) USING BTREE,
  KEY `fk_id_registro3` (`fk_id_registro`),
  CONSTRAINT `fk_id_escala3` FOREIGN KEY (`fk_id_escala`) REFERENCES `escala` (`id_escala`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_registro3` FOREIGN KEY (`fk_id_registro`) REFERENCES `registro` (`id_registro`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- ----------------------------
-- Records of registro_escala
-- ----------------------------

-- ----------------------------
-- Table structure for registro_nivel
-- ----------------------------
DROP TABLE IF EXISTS `registro_nivel`;
CREATE TABLE `registro_nivel` (
  `id_registro_nivel` int(11) NOT NULL AUTO_INCREMENT,
  `fk_id_registro` int(11) NOT NULL,
  `fk_id_factor` int(11) NOT NULL,
  `fk_id_nivel` int(11) NOT NULL,
  PRIMARY KEY (`id_registro_nivel`),
  KEY `fk_id_registro2` (`fk_id_registro`),
  KEY `fk_id_factor2` (`fk_id_factor`),
  KEY `fk_id_nivel2` (`fk_id_nivel`),
  CONSTRAINT `fk_id_factor2` FOREIGN KEY (`fk_id_factor`) REFERENCES `factor` (`id_factor`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_nivel2` FOREIGN KEY (`fk_id_nivel`) REFERENCES `nivel` (`id_nivel`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_registro2` FOREIGN KEY (`fk_id_registro`) REFERENCES `registro` (`id_registro`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of registro_nivel
-- ----------------------------

-- ----------------------------
-- Table structure for registro_valor
-- ----------------------------
DROP TABLE IF EXISTS `registro_valor`;
CREATE TABLE `registro_valor` (
  `id_registro_valor` int(11) NOT NULL AUTO_INCREMENT,
  `fk_id_dato` int(11) NOT NULL,
  `valor` decimal(36,18) DEFAULT NULL,
  `fk_id_tipo_valor` int(11) NOT NULL,
  `fk_id_registro` int(11) NOT NULL,
  `valor_previo` decimal(36,18) DEFAULT NULL,
  `fk_id_insercion` int(11) DEFAULT NULL,
  PRIMARY KEY (`id_registro_valor`),
  KEY `fk_id_dato1` (`fk_id_dato`),
  KEY `fk_id_tipo_valor1` (`fk_id_tipo_valor`),
  KEY `fk_id_registro1` (`fk_id_registro`),
  KEY `fk_id_insercion_rv_idx` (`fk_id_insercion`),
  CONSTRAINT `fk_id_dato1` FOREIGN KEY (`fk_id_dato`) REFERENCES `dato` (`id_dato`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_insercion_rv` FOREIGN KEY (`fk_id_insercion`) REFERENCES `inserciones` (`id_insert`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_id_registro1` FOREIGN KEY (`fk_id_registro`) REFERENCES `registro` (`id_registro`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_id_tipo_valor1` FOREIGN KEY (`fk_id_tipo_valor`) REFERENCES `tipo_valor` (`id_tipo_valor`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of registro_valor
-- ----------------------------

-- ----------------------------
-- Table structure for tipo_dato
-- ----------------------------
DROP TABLE IF EXISTS `tipo_dato`;
CREATE TABLE `tipo_dato` (
  `id_tipo_dato` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_tipo_dato` varchar(45) NOT NULL,
  PRIMARY KEY (`id_tipo_dato`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of tipo_dato
-- ----------------------------
INSERT INTO `tipo_dato` VALUES ('1', 'Plantae');
INSERT INTO `tipo_dato` VALUES ('2', 'Animalia');
INSERT INTO `tipo_dato` VALUES ('3', 'Fungi');
INSERT INTO `tipo_dato` VALUES ('4', 'Bacteria');

-- ----------------------------
-- Table structure for tipo_escala
-- ----------------------------
DROP TABLE IF EXISTS `tipo_escala`;
CREATE TABLE `tipo_escala` (
  `id_tipo_escala` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(200) COLLATE utf8_spanish_ci DEFAULT NULL,
  PRIMARY KEY (`id_tipo_escala`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- ----------------------------
-- Records of tipo_escala
-- ----------------------------
INSERT INTO `tipo_escala` VALUES ('1', 'Block');
INSERT INTO `tipo_escala` VALUES ('2', 'Plot');
INSERT INTO `tipo_escala` VALUES ('3', 'SubPlot');
INSERT INTO `tipo_escala` VALUES ('4', 'SubSubPlot');

-- ----------------------------
-- Table structure for tipo_experimento
-- ----------------------------
DROP TABLE IF EXISTS `tipo_experimento`;
CREATE TABLE `tipo_experimento` (
  `id_tipo_experimento` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_tipo_experimento` varchar(45) NOT NULL,
  PRIMARY KEY (`id_tipo_experimento`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of tipo_experimento
-- ----------------------------
INSERT INTO `tipo_experimento` VALUES ('1', 'Observational');
INSERT INTO `tipo_experimento` VALUES ('2', 'Experimental');

-- ----------------------------
-- Table structure for tipo_valor
-- ----------------------------
DROP TABLE IF EXISTS `tipo_valor`;
CREATE TABLE `tipo_valor` (
  `id_tipo_valor` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_tipo_valor` varchar(45) NOT NULL,
  `unidad_medida` varchar(45) DEFAULT NULL,
  `decimales` int(11) DEFAULT NULL,
  `mostrar` int(11) DEFAULT NULL,
  PRIMARY KEY (`id_tipo_valor`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of tipo_valor
-- ----------------------------

-- ----------------------------
-- Table structure for usuarios
-- ----------------------------
DROP TABLE IF EXISTS `usuarios`;
CREATE TABLE `usuarios` (
  `id_usuario` int(11) NOT NULL AUTO_INCREMENT,
  `nombre_usuario` varchar(70) NOT NULL,
  PRIMARY KEY (`id_usuario`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of usuarios
-- ----------------------------

-- ----------------------------
-- Table structure for valor_por_dato
-- ----------------------------
DROP TABLE IF EXISTS `valor_por_dato`;
CREATE TABLE `valor_por_dato` (
  `id_valor_dato` int(11) NOT NULL AUTO_INCREMENT,
  `fk_id_tipo_dato` int(11) NOT NULL,
  `fk_id_tipo_valor` int(11) NOT NULL,
  PRIMARY KEY (`id_valor_dato`),
  KEY `fk_dato` (`fk_id_tipo_dato`),
  KEY `fk_valor` (`fk_id_tipo_valor`),
  CONSTRAINT `fk_dato` FOREIGN KEY (`fk_id_tipo_dato`) REFERENCES `tipo_dato` (`id_tipo_dato`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `fk_valor` FOREIGN KEY (`fk_id_tipo_valor`) REFERENCES `tipo_valor` (`id_tipo_valor`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of valor_por_dato
-- ----------------------------
