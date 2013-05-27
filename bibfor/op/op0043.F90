subroutine op0043()
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
! person_in_charge: nicolas.greffet at edf.fr
    implicit   none
! =====================================================================
!   - FONCTIONS REALISEES:
!       COMMANDE IMPR_MAIL_YACS
!       RECUPERATION DU MAILLAGE FLUIDE LORS D'UN COUPLAGE ASTER-SATURNE
!       VIA YACS
!
!   - OUT :
!       IERR   : NON UTILISE
!     ------------------------------------------------------------
    include 'asterc/getvis.h'
    include 'asterc/getvtx.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/precou.h'
    include 'asterfort/ulisop.h'
    include 'asterfort/ulopen.h'
    integer :: nfis
    character(len=8) :: typema
    character(len=16) :: k16nom
    integer :: n
    integer :: iarg
!
    call jemarq()
    call infmaj()
!
    call getvis(' ', 'UNITE_MAILLAGE', 1, iarg, 1,&
                nfis, n)
! LECTURE DU MAILLAGE FLUIDE
    k16nom='                '
    if (ulisop ( nfis, k16nom ) .eq. 0) then
        call ulopen(nfis, ' ', 'FICHIER-MODELE', 'NEW', 'O')
    endif
    call getvtx(' ', 'TYPE_MAILLAGE', 1, iarg, 1,&
                typema, n)
    call precou(nfis, typema)
!
    write(nfis,*) 'FIN'
    rewind nfis
!
    call jedema()
end subroutine
