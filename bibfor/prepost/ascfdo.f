      SUBROUTINE ASCFDO ( UN, RM, RC, ALPHA, NBTRAN, EP1, EP2, EPI,
     +                    TETA1, TETA2, LTRAN, SUREP, LTCHAR, 
     +                    LTCLIM, TYPBOL, PAXEP, GAXEP, NT, NS, NC,
     +                    SFP, ORIEN, AZIM, RC0, RC2, RC3, POSIT, EPSI, 
     +                    NIVMAG, SYME )
      IMPLICIT    NONE
      INTEGER     UN, NBTRAN, NT, NS, NC, NIVMAG
      REAL*8      RM, RC, ALPHA, EP1, EP2, EPI, TETA1, TETA2, LTRAN,
     +            SUREP, LTCHAR, LTCLIM, PAXEP,
     +            GAXEP, SFP, AZIM, RC0, RC2, RC3, ORIEN, EPSI
      CHARACTER*8 TYPBOL, POSIT, SYME
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/03/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C TOLE  CRP_21
C     MACR_ASCOUF_MAIL
C
C     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
C                                                   "PLAQUE FISSURE"
C     DONNEES UTILISATEUR
C
C     ------------------------------------------------------------------
C 
      REAL*8        PI, R8PI
      CHARACTER*8   TYPEMB, POSIT2
      CHARACTER*128 NOMREP
      INTEGER       LNOM
C      
      PI = R8PI()
      TYPEMB = TYPBOL
      IF (TYPBOL.EQ.'CUVE')    TYPEMB = 'typcuv'
      IF (TYPBOL.EQ.'GV')      TYPEMB = 'typegv'
      IF (TYPBOL.EQ.'ASP_MPP') TYPEMB = 'typapp' 
      IF (POSIT.EQ.'DEB_INT ') THEN
         POSIT2 = 'interne'
      ELSE
         POSIT2 = 'externe'
      END IF
C
      WRITE(UN,10) '* DEBUT PARAMETRES UTILISATEUR'
      WRITE(UN,10) '*'
      WRITE(UN,*) 'c        = ',GAXEP/2.D0,';'
      WRITE(UN,*) 'a        = ',PAXEP,';'
      WRITE(UN,*) 'nt       = ',NT,';'
      WRITE(UN,*) 'ns       = ',NS,';'
      WRITE(UN,*) 'nc       = ',NC,';'
      WRITE(UN,*) 'rm       = ',RM,';'
      WRITE(UN,*) 'rc       = ',RC,';'
      WRITE(UN,*) 'alphac   = ',ALPHA,';'
      WRITE(UN,*) 'nbtranep = ',NBTRAN,';'
      WRITE(UN,*) 'ep1      = ',EP1,';'
      WRITE(UN,*) 'ep2      = ',EP2,';'
      WRITE(UN,*) 'epi      = ',EPI,';'
      WRITE(UN,*) 'teta1    = ',TETA1,';'
      WRITE(UN,*) 'teta2    = ',TETA2,';'
      WRITE(UN,*) 'ltran    = ',LTRAN,';'
      WRITE(UN,*) 'posfis   = ',SFP,';'
      WRITE(UN,*) 'ksiref   = ',ORIEN,';'
      WRITE(UN,*) 'surep    = ',SUREP,';'
      WRITE(UN,*) 'teta_f   = ',AZIM*PI/180.D0,';'
      WRITE(UN,*) 'rc0      = ',RC0,';'
      WRITE(UN,*) 'rc2      = ',RC2,';'    
      WRITE(UN,*) 'rc3      = ',RC3,';'
      WRITE(UN,*) 'pos      = ''',POSIT2(1:7),''';'
      WRITE(UN,*) 'lt       = ',LTCHAR,';'
      WRITE(UN,*) 'lgv      = ',LTCLIM,';'
      WRITE(UN,*) 'typembou = ''',TYPEMB(1:6),''';'
      IF (SYME(1:6).EQ.'ENTIER') THEN
        WRITE(UN,*) 'zsyme = ''entier'' ;'
      ELSE IF (SYME(1:5).EQ.'QUART') THEN
        WRITE(UN,*) 'zsyme = ''quart'' ;'
      ELSE
        WRITE(UN,*) 'zsyme = ''demi'' ;'
      END IF 
      WRITE(UN,*) 'epsif    = ',EPSI,';'      
      WRITE(UN,*) 'nivmag   = ',NIVMAG,';'      
      WRITE(UN,10) '*'
      WRITE(UN,10) '* FIN PARAMETRES UTILISATEUR'
C   
      CALL REPDEX(1,LNOM,NOMREP)
      WRITE (UN,10)
     +'opti donn '
      WRITE (UN,*)
     + ' '''//NOMREP(1:LNOM)//'ascouf_fiss_v4.datg''; '
C      WRITE (UN,10)
C     +' ''/exterieurs/aubhhmb/datg/ascouf_fiss_v4.datg'';'
     
 10   FORMAT(T1,A)
C
      END
