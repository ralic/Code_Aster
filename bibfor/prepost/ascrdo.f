      SUBROUTINE ASCRDO ( UN, RM, RC, ALPHA, EP, SUREP, LTCHAR, 
     +                    LTCLIM, TYPBOL, NBEP, TYPELE, NIVMAG )
      IMPLICIT    NONE
      INTEGER     UN, NBEP, NIVMAG
      REAL*8      RM, RC, ALPHA, EP, SUREP, LTCHAR, LTCLIM
      CHARACTER*8 TYPBOL
      CHARACTER*4 TYPELE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 23/02/2000   AUTEUR AUBHHMB M.BONNAMY 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE  CRP_20
C     MACR_ASCOUF_MAIL
C
C     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
C                                                   "PLAQUE REGLEE"
C     DONNEES UTILISATEUR
C
C     ------------------------------------------------------------------
C 
      INTEGER      NY,NZT,NZGV,NZC
      REAL*8       PI,R8PI,CZ,DELTAY,DELTAZ,DENEXT
      CHARACTER*8  TYPEMB     
      CHARACTER*128 NOMREP
      INTEGER       LNOM
C        
      PI = R8PI()
      CZ = ALPHA*RC*PI/180.D0
      NY = 20      
      NZC = INT((ALPHA+1.D-5)/5.D0)
      DELTAY = 2.D0*PI*RM/20
      DELTAZ = CZ/NZC
      DENEXT = INT(LTCHAR/DELTAY)/4.D0*DELTAY
       
C      IF (NZT.EQ.0) NZT = INT(NZC*LT/CZ)
C      IF (NZGV.EQ.0) NZGV = INT(NZC*LGV/CZ)
      NZT = 0
      NZGV= 0
C
      TYPEMB = TYPBOL
      IF (TYPBOL.EQ.'CUVE')    TYPEMB = 'typcuv'
      IF (TYPBOL.EQ.'GV')      TYPEMB = 'typegv'
      IF (TYPBOL.EQ.'ASP_MPP') TYPEMB = 'typapp'      
C      
      WRITE(UN,10) '* DEBUT PARAMETRES UTILISATEUR'
      WRITE(UN,10) '*'
      WRITE(UN,10) '* Parametres generaux'
      WRITE(UN,10) '*'
      WRITE(UN,*) 'nivmag   = ',NIVMAG,';'
      WRITE(UN,*) 'option dime 3 elem '//TYPELE//' nive nivmag echo 0;'
      WRITE(UN,*) 'rm       = ',RM,';'
      WRITE(UN,*) 'rc       = ',RC,';'
      WRITE(UN,*) 'alphac   = ',ALPHA,';'
      WRITE(UN,*) 'epc      = ',EP,';'
      WRITE(UN,*) 'surep    = ',SUREP,';'
      WRITE(UN,*) 'lgv      = ',LTCLIM,';'
      WRITE(UN,*) 'lt       = ',LTCHAR,';'      
      WRITE(UN,*) 'typembou = ''',TYPEMB(1:6),''';'
      WRITE(UN,*) 'nx       = ',NBEP,';'
      WRITE(UN,*) 'ny       = ',NY,';'
      WRITE(UN,*) 'pos      = ''bidon'';'
      WRITE(UN,*) 'l1       = 0. ;'
      WRITE(UN,*) 'lbloc    = 0. ;'      
      WRITE(UN,*) 'crit     = 0.0001;'
      WRITE(UN,*) 'crit2    = 0.01;'      
      WRITE(UN,*) 'epsit    = 1.e-3 ;'
      WRITE(UN,*) 'pirad    = ',PI,';'      
      WRITE(UN,*) 'nzc      = ',NZC,';'
      WRITE(UN,*) 'teta_f   = ',PI/2.D0,';'
      WRITE(UN,*) 'zpp31    = ',CZ,';'        
      WRITE(UN,*) 'daxbtu   =',DENEXT,';'
      WRITE(UN,*) 'daxhtu   =',DELTAZ,';'
      WRITE(UN,*) 'daxbgv   =',DELTAZ,';'
      WRITE(UN,*) 'daxhgv   =',DENEXT,';'
      WRITE(UN,*) 'nzt      =',-NZT,';'
      WRITE(UN,*) 'nzgv     =',-NZGV,';'
  
      WRITE(UN,10) '*'
      WRITE(UN,10) '* FIN PARAMETRES UTILISATEUR'
C         
      CALL REPDEX(1,LNOM,NOMREP)
      WRITE (UN,10)
     +'opti donn '
      WRITE (UN,*)
     + ' '''//NOMREP(1:LNOM)//'ascouf_regl_v1.datg''; '
C      WRITE (UN,10)
C     +' ''/exterieurs/aubhhmb/GIBI_ASCOUF/ascouf_regl_v1.datg''; '
C     
 10   FORMAT(T1,A)
C 
      END
