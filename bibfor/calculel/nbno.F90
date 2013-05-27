function nbno(mode)
    implicit none
    integer :: nbno
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ---------------------------------------------------------------------
!     BUT: TROUVER LE NOMBRE DE NOEUDS ASSOCIES A UN MODE_LOCAL
!     DE TYPE CHAM_NO, VECTEUR, OU MATRICE .
!
!     IN:  MODE : MODE_LOCAL
!     OUT: NBNO : NOMBRE DE NOEUDS
!
!
! ---------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    integer :: mode, m1, m2, n1, n2
!
!     FONCTIONS JEVEUX
!
!
!
!
!-----------------------------------------------------------------------
    integer :: iadm, iadm1, iadm2, ischn, ismat
!-----------------------------------------------------------------------
    call jemarq()
    ismat = 0
    ischn = 0
    call jeveuo(jexnum('&CATA.TE.MODELOC', mode), 'L', iadm)
!
    if (zi(iadm) .eq. 4) then
    else if (zi(iadm).eq.5) then
        ismat = 1
    else if (zi(iadm).eq.2) then
        ischn = 1
    else
        call u2mess('F', 'CALCULEL3_81')
    endif
!
    if (ischn .eq. 1) then
        nbno = zi(iadm-1+4)
        if (nbno .gt. 10000) nbno = nbno - 10000
        goto 9999
    endif
!
    m1 = zi(iadm+3)
    call jeveuo(jexnum('&CATA.TE.MODELOC', m1), 'L', iadm1)
    n1 = zi(iadm1+3)
    n1 = abs(n1)
    if (n1 .gt. 10000) then
        n1 = n1 - 10000
    endif
!
    if (ismat .eq. 1) then
        m2 = zi(iadm+4)
        call jeveuo(jexnum('&CATA.TE.MODELOC', m2), 'L', iadm2)
        n2 = zi(iadm2+3)
        n2 = abs(n2)
        if (n2 .gt. 10000) then
            n2 = n2 - 10000
        endif
        if (n1 .ne. n2) then
            call u2mess('F', 'CALCULEL3_82')
        endif
    endif
    nbno = n1
9999  continue
    call jedema()
end function
