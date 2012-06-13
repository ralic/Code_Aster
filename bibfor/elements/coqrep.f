      SUBROUTINE COQREP(PGL, ALPHA, BETA, T2EV, T2VE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      REAL*8 PGL(3,3), T2EV(*), T2VE(*), ALPHA, BETA
C     ---------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE SELLENET N.SELLENET
C     ------------------------------------------------------------------
C
C         CETTE ROUTINE EST UNE COPIE DE DXREPE
C         CALCUL DE LA MATRICE DE PASSAGE DU REPERE DE L'ELEMENT A
C         LA VARIETE (LE REPERE DE LA VARIETE EST OBTENU PAR LA MATRICE
C         DE PASSAGE GLOBAL -> LOCAL) AINSI QUE SON INVERSE
C
C         POUR TOUTES LES OPTIONS DE POST TRAITEMENT COQUE
C
C     ------------------------------------------------------------------
      REAL*8     DX, DY, DZ, S, C, NORM
      REAL*8     PS, PJDX, PJDY, PJDZ
      REAL*8     R8PREM
C
      DX = COS(BETA)*COS(ALPHA)
      DY = COS(BETA)*SIN(ALPHA)
      DZ = SIN(BETA)
      NORM = SQRT (DX*DX + DY*DY + DZ*DZ)
      DX = DX/NORM
      DY = DY/NORM
      DZ = DZ/NORM
      PS = DX*PGL(3,1) + DY*PGL(3,2) + DZ*PGL(3,3)
      PJDX = DX - PS*PGL(3,1)
      PJDY = DY - PS*PGL(3,2)
      PJDZ = DZ - PS*PGL(3,3)
      NORM = SQRT (PJDX*PJDX + PJDY*PJDY + PJDZ*PJDZ)
      IF ( NORM .LE. R8PREM() ) THEN
          CALL U2MESS('F','ELEMENTS_49')
      ENDIF
C
      PJDX = PJDX/NORM
      PJDY = PJDY/NORM
      PJDZ = PJDZ/NORM
      C = PJDX*PGL(1,1) + PJDY*PGL(1,2) + PJDZ*PGL(1,3)
      S = PJDX*PGL(2,1) + PJDY*PGL(2,2) + PJDZ*PGL(2,3)
C
      T2EV(1) =   C
      T2EV(2) =   S
      T2EV(3) = - S
      T2EV(4) =   C
C
      T2VE(1) =   C
      T2VE(2) = - S
      T2VE(3) =   S
      T2VE(4) =   C
C
      END
