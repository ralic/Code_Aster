subroutine ibdbgs()
    implicit none
!     ------------------------------------------------------------------
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
!     TRAITEMENT DES MOTS CLE DEBUG/MESURE_TEMPS/MEMOIRE
!     DES COMMANDES DEBUT ET POURSUITE
!     ------------------------------------------------------------------
!            0 TOUT C'EST BIEN PASSE
!            1 ERREUR DANS LA LECTURE DE LA COMMANDE
!     ------------------------------------------------------------------
!     ----- DEBUT COMMON DE DEBUG JEVEUX
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/jdcset.h'
    include 'asterfort/assert.h'
    include 'asterfort/impvem.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    integer :: lundef, idebug
    common /undfje/  lundef,idebug
    real(kind=8) :: tbloc, tgrel
    common /rtblje/  tbloc,tgrel
!
!     -- COMMON MESTP1 POUR MESURE_TEMPS
    integer :: mtpniv, mtpsta
    common /mestp1/  mtpniv,mtpsta
!
! ----------------------------------------------------------------------
    character(len=3) :: repons
    integer :: l, ncode, ndbg, iarg, i1
!
!     --- OPTIONS PAR DEFAUT ---
!-----------------------------------------------------------------------
    integer :: ifi
!-----------------------------------------------------------------------
    call jemarq()
    tbloc=800.d0
!
!     -- WARNING SUR LES MOTS-CLES CODE ET DEBUG
    call getfac('CODE', ncode)
    call getfac('DEBUG', ndbg)
    if (ncode .gt. 0 .or. ndbg .gt. 0) then
        call u2mess('I', 'SUPERVIS_22')
    endif
!
!     -- DEBUG / JXVERI :
    repons = 'NON'
    call getvtx('DEBUG', 'JXVERI', 1, iarg, 1,&
                repons, l)
    if (l .eq. 0) then
        if (repons .eq. 'OUI') then
            call u2mess('I', 'SUPERVIS_23')
!           LE "FLAG" JXVERI=OUI EST POSTIONNE DANS LE JDC
!           VOIR ROUTINE EXPASS.F
        endif
    endif
!
!     -- DEBUG / SDVERI :
    repons = 'NON'
    call getvtx('DEBUG', 'SDVERI', 1, iarg, 1,&
                repons, l)
    if (l .eq. 0) then
        if (ncode .gt. 0) then
!          UN JOUR, ON METTRA 'OUI' PAR DEFAUT ...
            repons='NON'
        else
            repons='NON'
        endif
    endif
!
    if (repons .eq. 'OUI') then
        call jdcset('sdveri', 1)
        call u2mess('I', 'SUPERVIS_24')
    else
        call jdcset('sdveri', 0)
    endif
!
!
!     -- DEBUG / JEVEUX :
!     -----------------------------------------------------
    repons = 'NON'
    call getvtx('DEBUG', 'JEVEUX', 1, iarg, 1,&
                repons, l)
    call assert(repons.eq.'OUI' .or. repons.eq.'NON')
    if (repons .eq. 'OUI') then
        call u2mess('I', 'SUPERVIS_12')
        idebug = 1
    endif
    call jdcset('jeveux', idebug)
!
!     -- DEBUG / ENVIMA :
!     -----------------------------------------------------
    repons = 'NON'
    call getvtx('DEBUG', 'ENVIMA', 1, iarg, 1,&
                repons, l)
    if (repons .eq. 'TES') then
        ifi = iunifi ( 'RESULTAT' )
        call impvem(ifi)
    endif
!
!
!     -- MESURE_TEMPS:
!     -----------------------------------------------------
    call getvis('MESURE_TEMPS', 'NIVE_DETAIL', 1, iarg, 1,&
                mtpniv, l)
    repons = 'NON'
    call getvtx('MESURE_TEMPS', 'MOYENNE', 1, iarg, 1,&
                repons, l)
    if (repons .eq. 'OUI') then
        mtpsta = 1
    else
        mtpsta = 0
    endif
!
!     -- MEMOIRE  :
!     -----------------------------------------------------
!
    call getvr8('MEMOIRE', 'TAILLE_BLOC', 1, iarg, 1,&
                tbloc, l)
    call getvis('MEMOIRE', 'TAILLE_GROUP_ELEM', 1, iarg, 1,&
                i1, l)
    tgrel=dble(i1)
!
    call jedema()
end subroutine
