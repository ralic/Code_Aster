subroutine rc36cm(iocc, etat, nbma, listma, nbchar,&
                  lichar, chmome)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/cesfus.h'
    include 'asterfort/cesqua.h'
    include 'asterfort/cesred.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/wkvect.h'
    integer :: iocc, nbma, listma(*), nbchar, lichar(*)
    character(len=1) :: etat
    character(len=24) :: chmome
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!     CALCUL DU TORSEUR PAR SOMMATION ALGEBRIQUE DES TORSEURS
!     CORRESPONDANT AUX DIFFERENTS CAS DE CHARGE DE LA SITUATION
!
! IN  : IOCC   : NUMERO D'OCCURRENCE DE SITUATION
! IN  : ETAT   : ETAT STABILISE A OU B POUR LE MESSAGE D'ERREUR
! IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
! IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
! IN  : NBCHAR : NOMBRE DE CAS DE CHARGE POUR UN ETAT STABILISE
! IN  : LICHAR : LISTE DES CAS DE CHARGE POUR UN ETAT STABILISE
! OUT : CHNOME : TORSEUR RESULTAT
!     ------------------------------------------------------------------
!
    integer :: jnume, jcham, nbresu, nbcmp, icha, ir, jlich, jlicm, jlicr
    integer :: vali(2)
    logical :: seisme, autre
    character(len=8) :: k8b, nocmp(3)
    character(len=24) :: chams0
    complex(kind=8) :: cbid
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call jeveuo('&&RC3600.NUME_CHAR', 'L', jnume)
    call jeveuo('&&RC3600.CHAMP', 'L', jcham)
    call jelira('&&RC3600.NUME_CHAR', 'LONMAX', nbresu, k8b)
!
    nbcmp = 3
    nocmp(1) = 'MT'
    nocmp(2) = 'MFY'
    nocmp(3) = 'MFZ'
!
    seisme = .false.
    autre = .false.
!
    call wkvect('&&RC36CM.LICH', 'V V K24', nbchar, jlich)
    call wkvect('&&RC36CM.LICM', 'V V L', nbchar, jlicm)
    call wkvect('&&RC36CM.LICR', 'V V R', nbchar, jlicr)
!
    do 110, icha = 1, nbchar, 1
    do 112, ir = 1, nbresu, 1
    if (lichar(icha) .eq. zi(jnume+ir-1)) goto 114
112  continue
    vali (1) = iocc
    vali (2) = lichar(icha)
    call u2mesi('F', 'POSTRCCM_28', 2, vali)
114  continue
    if (etat .eq. 'S') then
        seisme = .true.
    else
        autre = .true.
    endif
    zk24(jlich+icha-1) = zk24(jcham+ir-1)
    zl(jlicm+icha-1) = .true.
    zr(jlicr+icha-1) = 1.d0
    110 end do
!
    if (seisme .and. autre) then
        call u2mesi('F', 'POSTRCCM_29', 1, iocc)
    endif
!
    if (nbchar .eq. 1) then
        chams0 = zk24(jlich)
        call cesred(chams0, nbma, listma, nbcmp, nocmp,&
                    'V', chmome)
    else
!
        chams0='&&RC36CM.CHAMS0'
        if (autre) then
            call cesfus(nbchar, zk24(jlich), zl(jlicm), zr(jlicr), cbid,&
                        .false., 'V', chams0)
        else
            call cesqua(nbchar, zk24(jlich), zl(jlicm), 'V', chams0)
        endif
        call cesred(chams0, nbma, listma, nbcmp, nocmp,&
                    'V', chmome)
        call detrsd('CHAM_ELEM_S', chams0)
    endif
!
    call jedetr('&&RC36CM.LICH')
    call jedetr('&&RC36CM.LICM')
    call jedetr('&&RC36CM.LICR')
!
    call jedema()
end subroutine
