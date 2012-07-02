      SUBROUTINE MECHNC (NOMA,MOTCLE,IOCC,CHNUMC)
      IMPLICIT NONE
      INTEGER                        IOCC
      CHARACTER*(*)      NOMA,MOTCLE
      CHARACTER*24                        CHNUMC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     CREE UNE CARTE POUR LES COQUES
C     ------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : MOTCLE : MOTCLE FACTEUR
C IN  : IOCC   : NUMERO D'OCCURENCE
C OUT : CHNUMC : NOM DE LA CARTE CREEE
C     ------------------------------------------------------------------
      INTEGER      IVAL(3), NCOU, NANGL
      REAL*8       R8B
      CHARACTER*3  ORDO
      CHARACTER*8  K8B, LICMP(3)
      COMPLEX*16   C16B
      INTEGER      IARG
C DEB-------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER N1 ,N2 ,N3 ,NX3 
C-----------------------------------------------------------------------
      CALL GETVIS (MOTCLE,'NUME_COUCHE',IOCC,IARG,1,NCOU,N1)
      CALL GETVTX (MOTCLE,'NIVE_COUCHE',IOCC,IARG,1,ORDO,N2)
      CALL GETVIS (MOTCLE,'ANGLE',IOCC,IARG,1,NANGL,N3)
      CHNUMC = ' '
      IF ( N2 .NE. 0 ) THEN
         IF (ORDO.EQ.'SUP') THEN
            NX3 = 1
         ELSEIF (ORDO.EQ.'MOY') THEN
            NX3 = 0
         ELSEIF (ORDO.EQ.'INF') THEN
            NX3 = -1
         ENDIF
         CHNUMC = '&&MECHNC.NUMC'
         LICMP(1) = 'NUMC'
         LICMP(2) = 'ORDO'
         LICMP(3) = 'ANGL'
         IVAL(1) = NCOU
         IVAL(2) = NX3
         IVAL(3) = NANGL
         CALL MECACT('V',CHNUMC,'MAILLA',NOMA,'NUMC_I',3,LICMP,
     +                                               IVAL,R8B,C16B,K8B )
      ENDIF
C
      END
