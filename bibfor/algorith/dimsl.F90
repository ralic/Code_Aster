subroutine dimsl(ndim, nno, nnos, dimdef, dimcon,&
                 nnom, nnoc, nddls, nddlm, nddlc,&
                 dimuel, regula)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit      none
    integer :: ndim, nno, nnos, dimdef, dimcon, nnom, nnoc, nddls, nddlm
    integer :: nddlc, dimuel, regula(6)
! ======================================================================
! --- BUT : INITIALISATION DES GRANDEURS NECESSAIRES POUR LA GESTION ---
! ---       DU CALCUL AVEC REGULARISATION A PARTIR DU MODELE SECOND ----
! ---       GRADIENT A MICRO-DILATATION --------------------------------
! ======================================================================
    integer :: def1, def2, cont1, cont2
! ======================================================================
    nnoc = 0
    def1 = 1
    def2 = ndim
    dimdef = def1+def2
    cont1 = 1
    cont2 = ndim
    dimcon = cont1+cont2
! ======================================================================
! --- DIMENSION DU VECTEUR DES DEFORMATIONS GENERALISEES ---------------
! ======================================================================
! --- [E] = [DEPV,DGONFDX,DGONFDY,DGONFDZ] ------------------------
! ======================================================================
    nddls = ndim + 1
    nddlm = ndim
    nddlc = 0
! ======================================================================
    nnom = nno - nnos
    dimuel = nnos*nddls + nnom*nddlm
! ======================================================================
! --- POSITIONS DU POINTEUR REGULA : -----------------------------------
! --- (1) : ADRESSE DES DEFORMATIONS DEP*** ----------------------------
! --- (2) : ADRESSE DES DEFORMATIONS DGONFX* ---------------------------
! --- (3) : ADRESSE DES DEFORMATIONS PRES** ----------------------------
! --- (4) : ADRESSE DES CONTRAINTES GENERALISEES PRES** ----------------
! --- (5) : ADRESSE DES CONTRAINTES GENERALISEES SIG*** ----------------
! --- (6) : ADRESSE DES CONTRAINTES GENERALISEES DEP*** ----------------
! ======================================================================
    regula(1)=1
    regula(2)=regula(1)+def1
    regula(3)=regula(2)+def2
    regula(4)=1
    regula(5)=regula(4)+cont1
    regula(6)=regula(5)+cont2
! ======================================================================
end subroutine
