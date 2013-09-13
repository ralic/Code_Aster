subroutine regini(option, nomte, ivf, ivf2, idfde,&
                  idfde2, jgano, ndim, ipoids, ipoid2,&
                  npi, dimdef, nddls, nddlm, dimcon,&
                  typmod, dimuel, nno, nnom, nnos,&
                  regula, axi)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
    implicit none
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/utmess.h"
    logical :: axi
    integer :: ivf, ivf2, idfde, idfde2, jgano, ndim, ipoids, npi, nnom
    integer :: ipoid2, dimdef, dimuel, dimcon, nno, nnos, nddls, nddlm
    integer :: regula(6)
    character(len=8) :: typmod(2)
    character(len=16) :: option, nomte
! ======================================================================
! --- BUT : INITIALISATION DES GRANDEURS NECESSAIRES POUR LA GESTION ---
! ---       DU CALCUL AVEC REGULARISATION A PARTIR DU MODELE SECOND ----
! ---       GRADIENT ---------------------------------------------------
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: nno2, nnos2, npi2, ibid, def1, def2, def3, cont1, cont2, nnoc
    integer :: cont3, adder1, adder2, adder3, adcor1, adcor2, adcor3
    character(len=8) :: elrefe, elrf1, elrf2
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    typmod(2) = '        '
    elrf1 = '        '
    elrf2 = '        '
    axi = .false.
    def1 = 0
    def2 = 0
    def3 = 0
    cont1 = 0
    cont2 = 0
    cont3 = 0
    dimdef = 0
    dimcon = 0
    adder1 = 0
    adder2 = 0
    adder3 = 0
    adcor1 = 0
    adcor2 = 0
    adcor3 = 0
! ======================================================================
! --- TYPE D'ELEMENT ---------------------------------------------------
! ======================================================================
    call elref1(elrefe)
    if (elrefe .eq. 'TR7') then
        elrf1 = 'TR6'
        elrf2 = 'TR3'
    else if (elrefe.eq.'QU9') then
        elrf1 = 'QU8'
        elrf2 = 'QU4'
    else if (elrefe.eq.'H27') then
        elrf1 = 'H20'
        elrf2 = 'HE8'
    else
        call utmess('F', 'DVP_4', sk=elrefe)
    endif
! ======================================================================
! --- FONCTIONS DE FORME P2 --------------------------------------------
! ======================================================================
    call elref4(elrf1, 'RIGI', ndim, nno, nnos,&
                npi, ipoids, ivf, idfde, jgano)
! ======================================================================
! --- FONCTIONS DE FORME P1 --------------------------------------------
! ======================================================================
    call elref4(elrf2, 'RIGI', ndim, nno2, nnos2,&
                npi2, ipoid2, ivf2, idfde2, ibid)
! ======================================================================
! --- NNOC DESIGNE LE NOMBRE DE NOEUD AU CENTRE DES ELEMENTS -----------
! ======================================================================
! --- [E] = [DEPVIJ,DVIJDX,DVIJDY,LAMBIJ] ------------------------------
! ======================================================================
    nnoc = 1
    def1 = ndim*ndim
    def2 = ndim*ndim*ndim
    def3 = ndim*ndim
    dimdef = def1+def2+def3
    cont1 = ndim*ndim
    cont2 = ndim*ndim*ndim
    cont3 = ndim*ndim
    dimcon = cont1+cont2+cont3
! ======================================================================
! --- RECUPERATION DU TYPE DE LA MODELISATION --------------------------
! ======================================================================
    typmod(1) = 'D_PLAN  '
! ======================================================================
! --- DIMENSION DES COMPOSANTES NODALES --------------------------------
! ======================================================================
    nddls = ndim + ndim*ndim
    nddlm = ndim
! ======================================================================
    adder1 = 1
    adder2 = adder1+def1
    adder3 = adder2+def2
    adcor1 = 1
    adcor2 = adcor1+cont1
    adcor3 = adcor2+cont2
    nnom = nno - nnos
    dimuel = nnos*nddls + nnom*nddlm + nnoc*ndim*ndim
! ======================================================================
! --- POSITIONS DU POINTEUR REGULA : -----------------------------------
! --- (1) : ADRESSE DES DEFORMATIONS DEPV** ----------------------------
! --- (2) : ADRESSE DES DEFORMATIONS DGONFX* ---------------------------
! --- (3) : ADRESSE DES DEFORMATIONS PRES** ----------------------------
! --- (4) : ADRESSE DES CONTRAINTES GENERALISEES PRES** ----------------
! --- (5) : ADRESSE DES CONTRAINTES GENERALISEES SIG*** ----------------
! --- (6) : ADRESSE DES CONTRAINTES GENERALISEES DEPV** ----------------
! ======================================================================
    regula(1)=adder1
    regula(2)=adder2
    regula(3)=adder3
    regula(4)=adcor1
    regula(5)=adcor2
    regula(6)=adcor3
! ======================================================================
end subroutine
