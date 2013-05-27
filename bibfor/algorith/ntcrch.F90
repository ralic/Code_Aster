subroutine ntcrch(modele, numedd, hydr0, vhydr)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/carces.h'
    include 'asterfort/cescel.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mecact.h'
    include 'asterfort/vtcreb.h'
    character(len=24) :: modele
    character(len=24) :: numedd
    character(len=24) :: hydr0, vhydr
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_NON_LINE (ALGORITHME - INITIALISATIONS)
!
! CREATION DES VECTEURS D'INCONNUS
!
! ----------------------------------------------------------------------
!
! IN  NUMEDD : NUME_DDL
!
!
!
!
    character(len=19) :: hydric, hydris, ligrmo
    integer :: ibid, nncp, iret, neq
    character(len=8) :: k8bid
    character(len=24) :: vtemp
    complex(kind=8) :: c16bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    hydric = '&&NTCRCH.HYDR_C'
    hydris = '&&NTCRCH.HYDR_S'
    hydr0 = '&&NTCRCH.HYDR0'
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrmo, iret)
!
! --- CREATION CHAMP HYDRATATION
!
    if (vhydr .ne. ' ') then
        call mecact('V', hydric, 'MODELE', ligrmo, 'HYDR_R',&
                    1, 'HYDR', ibid, 0.d0, c16bid,&
                    k8bid)
        call carces(hydric, 'ELNO', ' ', 'V', hydris,&
                    'A', iret)
        call cescel(hydris, ligrmo, 'RESI_RIGI_MASS', 'PHYDRPP', 'NON',&
                    nncp, 'V', hydr0, 'F', ibid)
        call copisd('CHAMP_GD', 'V', hydr0, vhydr)
    endif
!
! --- CREATION DES CHAMPS
!
    vtemp='&&NXLECTVAR_____'
    call vtcreb(vtemp, numedd, 'V', 'R', neq)
!
    call detrsd('CHAMP', hydric)
    call detrsd('CHAMP', hydris)
!
    call jedema()
end subroutine
