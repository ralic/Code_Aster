subroutine recupe(noma, ndim, nk1d, lrev, matrev,&
                  deklag, prodef, londef, oridef)
    implicit     none
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    integer :: ndim, nk1d
    real(kind=8) :: lrev, deklag, prodef, londef
    character(len=8) :: noma, matrev, oridef
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
!
! ======================================================================
! ======================================================================
! --- BUT : RECUPERATION DES DONNEES DE LA COMMANDE POST_K_BETA --------
! ======================================================================
! OUT : NOMA   : NOM DU MAILLAGE ---------------------------------------
! --- : NDIM   : DIMENSION DE L'ESPACE ---------------------------------
! --- : NK1D   : NOMBRE D'OCCURENCE ------------------------------------
! --- : LREV   : LONGUEUR DU REVETEMENT --------------------------------
! --- : MATREV : MATERIAU DU REVETEMENT --------------------------------
! --- : PRODEF : PROFONDEUR DU DEFAUT ----------------------------------
! --- : LONDEF : LONGUEUR DU DEFAUT ------------------------------------
! --- : ORIDEF : ORIENTATION DU DEFAUT ---------------------------------
! ======================================================================
    integer :: ibid, ier
    character(len=8) :: k8b
    character(len=16) :: motfac
    integer :: iarg
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DU MAILLAGE -----------------------------------------
! ======================================================================
    call getvid(' ', 'MAILLAGE', 1, iarg, 1,&
                noma, ibid)
! ======================================================================
! --- DIMENSION DE L'ESPACE --------------------------------------------
! ======================================================================
    call dismoi('F', 'Z_CST', noma, 'MAILLAGE', ibid,&
                k8b, ier)
    if (k8b(1:3) .eq. 'OUI') then
        ndim = 2
    else
        ndim = 3
    endif
! ======================================================================
! --- RECUPERATION DES CARACTERISTIQUES DU REVETEMENT ------------------
! ======================================================================
    call getvr8(' ', 'EPAIS_REV', 1, iarg, 1,&
                lrev, ibid)
    call getvid(' ', 'MATER_REV', 1, iarg, 1,&
                matrev, ibid)
! ======================================================================
! --- RECUPERATION DES DONNEES DE LA FISSURE ---------------------------
! ======================================================================
    call getvr8('FISSURE', 'DECALAGE', 1, iarg, 1,&
                deklag, ibid)
    if (deklag .gt. 0.0d0) then
        call u2mess('F', 'PREPOST4_60')
    endif
    call getvr8('FISSURE', 'PROFONDEUR', 1, iarg, 1,&
                prodef, ibid)
    call getvr8('FISSURE', 'LONGUEUR', 1, iarg, 1,&
                londef, ibid)
    call getvtx('FISSURE', 'ORIENTATION', 1, iarg, 1,&
                oridef, ibid)
! ======================================================================
! --- RECUPERATION DU NOMBRE D'OCCURENCE DE K1D ------------------------
! ======================================================================
    motfac = 'K1D'
    call getfac(motfac, nk1d)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
