subroutine recyec(nmresz, mdcycz, numsec, typsdz)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    P. RICHARD     DATE 16/04/91
!-----------------------------------------------------------------------
!  BUT:      < RESTITUTION CYCLIQUE ECLATEE >
    implicit none
!
!      RESTITUER EN BASE PHYSIQUE SUR UN SECTEUR LES RESULTATS
!                ISSU DE LA SOUS-STRUCTURATION CYCLIQUE
!  LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "MODE_MECA"
!
!-----------------------------------------------------------------------
!
! NMRESZ   /I/: NOM K8 DU CONCEPT MODE MECA RESULTAT
! MDCYCZ   /I/: NOM K8 MODE_CYCL AMONT
! NUMSEC   /I/: NUMERO DU SECTEUR SUR LEQUEL RESTITUER
! TYPSDZ   /I/: TYPE STRUCTURE DONNEE RESULTAT (MODE_MECA,BASE_MODALE)
!
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/recbec.h'
    include 'asterfort/remnec.h'
    include 'asterfort/titre.h'
    character(len=8) :: nomres, modcyc, basmod, typint
    character(len=*) :: nmresz, mdcycz, typsdz
    character(len=16) :: typsd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------ECRITURE DU TITRE-------------------------------------
!
!-----------------------------------------------------------------------
    integer :: llref, lltyp, numsec
!-----------------------------------------------------------------------
    call jemarq()
    nomres = nmresz
    modcyc = mdcycz
    typsd = typsdz
!
    call titre()
!
!-------------------RECUPERATION DE LA BASE MODALE----------------------
!
    call jeveuo(modcyc//'.CYCL_REFE', 'L', llref)
    basmod=zk24(llref+2)
!
!-----------------------RECUPERATION DU TYPE INTERFACE------------------
!
!
    call jeveuo(modcyc//'.CYCL_TYPE', 'L', lltyp)
    typint=zk8(lltyp)
!
!
!------------------------------RESTITUTION -----------------------------
!
!    CAS CRAIG-BAMPTON ET CRAIG-BAMPTON HARMONIQUE
!
    if (typint .eq. 'CRAIGB  ' .or. typint .eq. 'CB_HARMO') then
        call recbec(nomres, typsd, basmod, modcyc, numsec)
    endif
!
!
!    CAS MAC NEAL AVEC ET SANS CORRECTION
!
    if (typint .eq. 'MNEAL   ' .or. typint .eq. 'AUCUN   ') then
        call remnec(nomres, typsd, basmod, modcyc, numsec)
    endif
!
!
    call jedema()
end subroutine
