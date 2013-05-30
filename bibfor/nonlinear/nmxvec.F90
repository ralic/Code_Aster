subroutine nmxvec(modelz, mate, carele, compor, carcri,&
                  sdtime, sddisc, sddyna, numins, valinc,&
                  solalg, lischa, comref, resoco, resocu,&
                  numedd, parcon, veelem, veasse, measse,&
                  nbvect, ltypve, lcalve, loptve, lassve)
!
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
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_21
!
    implicit      none
    include 'asterfort/assert.h'
    include 'asterfort/diinst.h'
    include 'asterfort/nmassv.h'
    include 'asterfort/nmcalv.h'
    include 'asterfort/nmchex.h'
    integer :: nbvect
    character(len=6) :: ltypve(20)
    logical :: lcalve(20), lassve(20)
    character(len=16) :: loptve(20)
    character(len=*) :: modelz
    character(len=24) :: mate, carele, sdtime
    character(len=24) :: compor, carcri, numedd
    integer :: numins
    real(kind=8) :: parcon(*)
    character(len=19) :: sddisc, sddyna, lischa
    character(len=24) :: resoco, resocu, comref
    character(len=19) :: veelem(*), veasse(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL ET ASSEMBLAGE DES VECT_ELEM DE LA LISTE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  PARCON : PARAMETRES DU CRITERE DE CONVERGENCE REFERENCE
!                     1 : SIGM_REFE
!                     2 : EPSI_REFE
!                     3 : FLUX_THER_REFE
!                     4 : FLUX_HYD1_REFE
!                     5 : FLUX_HYD2_REFE
!                     6 : VARI_REFE
!                     7 : EFFORT (FORC_REFE)
!                     8 : MOMENT (FORC_REFE)
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  RESOCU : SD RESOLUTION LIAISON_UNILATER
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  SOLVEU : SOLVEUR
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  NBVECT : NOMBRE DE VECT_ELEM DANS LA LISTE
! IN  LTYPVE : LISTE DES NOMS DES VECT_ELEM
! IN  LASSVE : SI VECT_ELEM A ASSEMBLER
! IN  LCALVE : SI VECT_ELEM A CALCULER
! IN  LOPTVE : OPTION DE CALCUL DES VECT_ELEM
!
! ----------------------------------------------------------------------
!
    character(len=6) :: typvec
    integer :: ivect
    character(len=19) :: vecele, vecass
    real(kind=8) :: instam, instap
    character(len=24) :: modele
    logical :: lcalc, lasse
    character(len=16) :: option
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    if (numins .eq. 0) then
        instam = 0.d0
        instap = diinst(sddisc,numins)
    else
        instam = diinst(sddisc,numins-1)
        instap = diinst(sddisc,numins)
    endif
    modele = modelz
!
! --- CALCUL ET ASSEMBLAGE DES VECT_ELEM
!
    do 10 ivect = 1, nbvect
!
! --- VECT_ELEM COURANT
!
        typvec = ltypve(ivect)
        lcalc = lcalve(ivect)
        lasse = lassve(ivect)
        option = loptve(ivect)
!
! --- UTILISER NMFINT
!
        if (typvec .eq. 'CNFINT') then
            call assert(.false.)
        endif
!
! --- UTILISER NMDIRI
!
        if (typvec .eq. 'CNDIRI') then
            call assert(.false.)
        endif
!
! --- UTILISER NMBUDI
!
        if (typvec .eq. 'CNBUDI') then
            call assert(.false.)
        endif
!
! --- CALCULER VECT_ELEM
!
        if (lcalc) then
            call nmchex(veelem, 'VEELEM', typvec, vecele)
            call nmcalv(typvec, modele, lischa, mate, carele,&
                        compor, carcri, numedd, comref, sdtime,&
                        parcon, instam, instap, valinc, solalg,&
                        sddyna, option, vecele)
        endif
!
! --- ASSEMBLER VECT_ELEM
!
        if (lasse) then
            call nmchex(veasse, 'VEASSE', typvec, vecass)
            call nmassv(typvec, modelz, lischa, mate, carele,&
                        compor, numedd, instam, instap, resoco,&
                        resocu, sddyna, sdtime, valinc, comref,&
                        measse, vecele, vecass)
        endif
10  end do
!
end subroutine
