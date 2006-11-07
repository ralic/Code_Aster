      SUBROUTINE CRGDM (IMATE,COMPOR,T,LAMBDA,DEUXMU,
     &                  LAMF,DEUMUF,GT,GC,GF,SEUIL,ALPHA,EP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/11/2006   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE
      CHARACTER*16       COMPOR(*)
      INTEGER            IMATE,T(2,2)
      REAL*8             LAMBDA, DEUXMU,DEUMUF, LAMF
      REAL*8             GT,GC,GF,SEUIL,ALPHA
C ----------------------------------------------------------------------
C
C BUT : LECTURE DES PARAMETRES MATERIAU POUR LE MODELE GLRC_DM
C       
C
C IN:
C       IMATE   : ADRESSE DU MATERIAU
C       COMPOR  : COMPORTMENT
C       EP      : EPAISSEUR DE LA PLAQUE
C OUT:
C       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
C       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
C       LAMF    : PARAMETRE D ELASTICITE - FLEXION
C       DEUMUF  : PARAMETRE D ELASTICITE - FLEXION 
C       D2      : ET DE L AUTRE 
C       GT      : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION 
C       GC      : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
C       GF      : PARAMETRE GAMMA POUR LA FLEXION
C       SEUIL   : INITIAL MEMBRANE
C       ALPHA   : PARAMETRE DE SEUIL FLEXION
C ----------------------------------------------------------------------
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(5)
      INTEGER     I,K,ICARA
      REAL*8      VALRES(5), E, NU, EP,EFLEX
      REAL*8      TREPSM,EPS(6),DSIGM,DSIGF,SYT,SYF,NUF

      T(1,1)=1
      T(1,2)=3
      T(2,1)=3
      T(2,2)=2

      IF ((.NOT.( COMPOR(1)(1:15) .EQ. 'GLRC_DM'))) THEN
            CALL UTMESS('F','GLRC_DM',
     &           ' COMPORTEMENT INATTENDU : '//COMPOR(1))
      ENDIF

C    LECTURE DES CARACTERISTIQUES DU MATERIAU
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'

      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET,'FM')

      E     = VALRES(1)
      NU    = VALRES(2)

      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)*EP
      DEUXMU = E/(1.D0+NU)*EP

      NOMRES(1) = 'EF'
      NOMRES(2) = 'NUF'
      CALL RCVALA(IMATE,' ','GLRC_DM',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, ' ')

      IF(CODRET(1).EQ.'OK') THEN
        EFLEX = VALRES(1)
      ELSE
        EFLEX = E
      ENDIF       
        
      IF(CODRET(2).EQ.'OK') THEN
        NUF = VALRES(2)
      ELSE
        NUF = NU
      ENDIF       
        
        
      LAMF    = EFLEX*NUF/(1.D0-NUF*NUF) *EP**3/12.0D0
      DEUMUF = EFLEX/(1.D0+NUF) *EP**3/12.0D0


C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'GAMMA_T'
      NOMRES(2) = 'GAMMA_F'
      NOMRES(3) = 'SYT'
      NOMRES(4) = 'SYF'
      CALL RCVALA(IMATE,' ','GLRC_DM',0,' ',0.D0,4,
     &            NOMRES,VALRES,CODRET,' ')
     
      GC     = 1.0D0
      GT = VALRES(1)
      GF = VALRES(2)
      SYT   = VALRES(3)
      SYF   = VALRES(4)
C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'GAMMA_T'
      NOMRES(2) = 'GAMMA_F'
      NOMRES(3) = 'SYT'
      NOMRES(4) = 'SYF'
      CALL RCVALA(IMATE,' ','GLRC_DM',0,' ',0.D0,4,
     &            NOMRES,VALRES,CODRET,' ')
     
      GC     = 1.0D0
      GT = VALRES(1)
      GF = VALRES(2)

      SEUIL = LAMBDA*(1.0D0-2.0D0*NU)**2 + DEUXMU*(1.0D0+2.0D0*NU**2)
      SEUIL = SEUIL/(2.0D0*(LAMBDA*(1.0D0-2.0D0*NU) + DEUXMU))**2
      SEUIL = SEUIL*SYT**2

      ALPHA = LAMF*(1.0D0-NUF)**2 + DEUMUF*(1.0D0+NUF**2)
      ALPHA = ALPHA/(2.0D0*(LAMF*(1.0D0-NUF) + DEUMUF))**2
      ALPHA = ALPHA*SYF**2/SEUIL

      END
