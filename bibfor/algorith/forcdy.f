      SUBROUTINE FORCDY( MASSE , AMORT , LAMORT ,
     +                    NEQ,
     +                    C0 , C1 , C2 , C3 , C4 , C5 ,
     +                    D0 , V0 , A0 , F1 , F2 , F )
C**********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C   BUT :      CALCUL DU VECTEUR FORCE DYNAMIQUE
C
C              F  = F  + M*(C0.D0+C1.V0+C2.A0)
C                      + C*(C3.D0+C4.V0+C5.A0)
C  ======
C
C
C   INPUT:
C   ---> MASSE   : POINTEUR DE LA MATRICE MASSE
C   ---> AMORT   : POINTEUR DE LA MATRICE AMORTISS
C   ---> LAMORT  : VARIABLE LOGIQUE
C                     .TRUE. SI IL Y A UNE MATRICE AMORTISSEMENT
C                     .FALSE. SINON
C   ---> C0,C1,C2,C3,C4,C5 : CONSTANTES DE CALCUL
C   ---> NEQ   : NOMBRE D'EQUATIONS (D.D.L. ACTIFS)
C   ---> D0    : VECTEUR DEPLACEMENT  INITIAL  (NEQ)
C   ---> V0    : VECTEUR VITESSE      INITIAL  (NEQ)
C   ---> A0    : VECTEUR ACCELERATION INITIAL  (NEQ)
C   ---> F1    : VECTEUR REEL DE TRAVAIL        (NEQ)
C   ---> F2    : VECTEUR REEL DE TRAVAIL        (NEQ)
C
C   VAR   :
C   <--> F     : VECTEUR FORCE EXTERIEURE ENTREE (NEQ)
C                VECTEUR FORCE DYNAMIQUE SORTIE (NEQ)
C
C ----------------------------------------------------------------------
      IMPLICIT REAL *8 (A-H,O-Z)
      REAL *8      D0(*) , V0(*) , A0(*) , F1(*), F2(*), F(*)
      REAL *8      C0 , C1 , C2 , C3 , C4 , C5
      LOGICAL      LAMORT
      INTEGER      MASSE,AMORT
C ---------------------------------------------------------------------
C     INPUT:
C
C     MASSE        :    NOM MATRICE MASSE
C     AMORT        :    NOM MATRICE AMORTISSEMENT
C     LAMORT       :    =.TRUE. SI AMORTISSEMENT
C ---------------------------------------------------------------------
C
      REAL *8    ZERO ,UN
C
      ZERO  = 0.D0
      UN    = 1.D0
      CALL R8INIR ( NEQ , ZERO , F1 , 1 )
      CALL R8AXPY ( NEQ , C0 , D0 , 1 , F1 , 1 )
      CALL R8AXPY ( NEQ , C1 , V0 , 1 , F1 , 1 )
      CALL R8AXPY ( NEQ , C2 , A0 , 1 , F1 , 1 )
      CALL MRMULT('ZERO', MASSE , F1 ,'R',  F2 , 1 )
      CALL R8AXPY ( NEQ , UN , F2 , 1 , F , 1 )
      IF (LAMORT) THEN
         CALL R8INIR ( NEQ , ZERO , F1 , 1  )
         CALL R8AXPY ( NEQ , C3 , D0 , 1 , F1 , 1 )
         CALL R8AXPY ( NEQ , C4 , V0 , 1 , F1 , 1 )
         CALL R8AXPY ( NEQ , C5 , A0 , 1 , F1 , 1 )
         CALL MRMULT('ZERO', AMORT , F1 , 'R', F2 , 1 )
         CALL R8AXPY ( NEQ , UN , F2 ,  1 , F , 1 )
      ENDIF
      END
