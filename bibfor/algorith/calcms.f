        SUBROUTINE CALCMS( NBPHAS,NBFSYM,NBCOMM,CPMONO,NMAT,PGL2,
     &                     COEFT,TOUTMS )
        IMPLICIT NONE
        INTEGER NMAT,NBCOMM(NMAT,3),NBFSYM,NBPHAS
        REAL*8 PGL(3,3),TOUTMS(NBPHAS,NBFSYM,12,6),COEFT(NMAT)  

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/11/2004   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C RESPONSABLE JMBHH01 J.M.PROIX
C       IN 
C         NBCOMM :  NOMBRE DE COEF COEFTIAU PAR FAMILLE
C         CPMONO :  NOMS DES LOIS COEFTIAU PAR FAMILLE
C   PGL2   : MATRICE DE PASSAGE GLOBAL LOCAL DU POLYCRSTAL (a faire)
C     OUT:
C           TOUTMS  :  TOUS LES TENSEURS MS
C INTEGRATION DES LOIS POLYCRISTALLINES PAR UNE METHODE DE RUNGE KUTTA
C
C     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
C     INTERNES DU MODELE
C
C     ----------------------------------------------------------------
      CHARACTER*8 MOD
      CHARACTER*16 NOMFAM,NCOEFT,NECOUL,NECRIS,NECRCI,CPMONO(5*NMAT+1)
      REAL*8 ANG(3),PGL2(3,3),R8DGRD,MS(6),WORK(3,3)
      INTEGER NBFSYS,I,IFA,NBSYS,IS,INDORI,INDCP
      INTEGER INDPHA,IPHAS,INDFA
C     ----------------------------------------------------------------

C         CALCUl DES TENSEURS MS POUR GAGNER DU TEMPS
      DO 1 IPHAS=1,NBPHAS         
C        INDPHA indice debut phase IPHAS dans NBCOMM
         INDPHA=NBCOMM(1+IPHAS,1)
C         recuperer l'orientation de la phase et la proportion
         INDORI=NBCOMM(1+IPHAS,3)+1
         ANG(1)=COEFT(INDORI)*R8DGRD()
         ANG(2)=COEFT(INDORI+1)*R8DGRD()
         ANG(3)=COEFT(INDORI+2)*R8DGRD()
         CALL MATROT(ANG,PGL)
         NBFSYS=NBCOMM(INDPHA,1)
         INDCP=NBCOMM(1+IPHAS,2)     
         IF (NBFSYS.GT.NBFSYM) THEN
            CALL UTMESS('F','CALCMS','TROP DE FAMILLES DE SYSTEMES'//
     &                  ' DE GLISSEMENT. MODIFIER GERPAS')            
         ENDIF
C        Nombre de variables internes de la phase (=monocristal)        
         DO 2 IFA=1,NBFSYS            
            NOMFAM=CPMONO(INDCP+5*(IFA-1)+1)
            CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS)         
            IF (NBSYS.EQ.0) CALL UTMESS('F','LCMMON','NBSYS=0')
C           indice de la famille IFA            
C            INDFA=INDPHA+IFA
            
            DO 3 IS=1,NBSYS
C              CALCUL DE LA SCISSION REDUITE =
C              PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
C              TAU      : SCISSION REDUITE TAU=SIG:MS
               CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS)
               DO 4 I=1,6
                  TOUTMS(IPHAS,IFA,IS,I)=MS(I)
 4             CONTINUE
 3          CONTINUE
 2       CONTINUE

 1    CONTINUE
      END
