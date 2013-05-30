subroutine rcmfmc(chmatz, chmacz)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/ismaem.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/copisd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jeveut.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rcmaco.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: chmatz, chmacz
! ----------------------------------------------------------------------
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
!
!     CREATION DE LA CARTE DU MATERIAU CODE A PARTIR DU CHAMP_MATER
!
! IN  CHMATZ    : CHAM_MATER
! OUT CHMACZ    : CARTE DE MATERIAU CODE
! ----------------------------------------------------------------------
!
!
    integer :: nbcvrc, jvcnom
    common /caii14/nbcvrc,jvcnom
!
    integer :: nbval, jchev, iret, jvale, igd, jdesc, kk
    integer :: nbgrp, i, icompt, igrp, ingrp, nbcmp, ier, j, k, nbmat
    integer :: inbmat
    character(len=4) :: knumat, kbid
    character(len=8) :: k8b, chmat, nomgd
    character(len=19) :: codi
    character(len=19) :: chemat, chmace
!     ------------------------------------------------------------------
!
    call jemarq()
!     CALL IMPTOU(' ','ALL','ALL')
!
    chmat=chmatz
    chemat=chmat//'.CHAMP_MAT'
    chmace=chmat//'.MATE_CODE'
!
    call jelira(chemat//'.VALE', 'LONMAX', nbval, k8b)
    call jeveuo(chemat//'.VALE', 'L', jchev)
!
!
!     -- SI CHMACE EXISTE, C'EST QUE L'ON A DEJA APPELE
!        LA ROUTINE RCMFMC. TOUT EST DEJA FAIT :
!     ----------------------------------------------------------
    call exisd('CARTE', chmace, iret)
    if (iret .gt. 0) goto 90
!
!
!     -- MISE A JOUR DU COMON CAII14 POUR RCVARC :
!     -----------------------------------------------
    call jeexin(chmat//'.CVRCNOM', iret)
    if (iret .ne. 0) then
        call jeveut(chmat//'.CVRCNOM', 'L', jvcnom)
        call jelira(chmat//'.CVRCNOM', 'LONMAX', nbcvrc, kbid)
    else
        nbcvrc=0
        jvcnom=ismaem()
    endif
!
!
!     -- TRAITEMENT DU MATERIAU PAR ELEMENTS :
!     -----------------------------------------------
    call jeveuo(chemat//'.DESC', 'L', jdesc)
    call jenuno(jexnum('&CATA.GD.NOMCMP', zi(jdesc)), nomgd)
    call dismoi('F', 'NB_CMP_MAX', nomgd, 'GRANDEUR', nbcmp,&
                k8b, ier)
    call assert(nbcmp.ge.30)
    call assert((nbval/nbcmp)*nbcmp.eq.nbval)
!
    call copisd('CHAMP_GD', 'V', chemat, chmace)
    call jedetr(chmace//'.VALE')
    nbgrp=nbval/nbcmp
    call wkvect(chmace//'.VALE', 'V V I', nbgrp, jvale)
    call jenonu(jexnom('&CATA.GD.NOMGD', 'ADRSJEVE'), igd)
    call jeveuo(chmace//'.DESC', 'E', jdesc)
    zi(jdesc)=igd
!
!
!     --- CODAGE DU MATERIAU ---
!     -------------------------------------------------
    icompt=0
    do 10 i = 1, nbval
        if (zk8(jchev-1+i) .ne. ' ') icompt=icompt+1
10  end do
    call assert(icompt.gt.0)
!
    call jedetr(chmat//'.MATE_CODE.GRP')
    call jedetr(chmat//'.MATE_CODE.NGRP')
    call wkvect(chmat//'.MATE_CODE.GRP', 'V V K8', icompt, igrp)
    call wkvect(chmat//'.MATE_CODE.NGRP', 'V V I', nbgrp, ingrp)
!
    icompt=0
    inbmat=0
    do 40 i = 1, nbgrp
!        -- IL NE PEUT PAS Y AVOIR PLUS DE 28 MATERIAUX
        do 20 j = 1, 28
            k=(i-1)*nbcmp+j
            if (zk8(jchev-1+k) .eq. 'TREF=>') goto 30
            if (zk8(jchev-1+k) .ne. ' ') then
                zk8(igrp+icompt)=zk8(jchev-1+k)
                icompt=icompt+1
                inbmat=inbmat+1
            endif
20      continue
30      continue
        zi(ingrp-1+i)=inbmat
        inbmat=0
40  end do
!
    codi=' '
    call jeveuo(chmat//'.MATE_CODE.GRP', 'L', igrp)
    call jeveuo(chmat//'.MATE_CODE.NGRP', 'L', ingrp)
    icompt=0
    do 50 kk = 1, nbgrp
        nbmat=zi(ingrp-1+kk)
        if (nbmat .eq. 0) goto 50
        call rcmaco(chmat(1:8), icompt, nbmat, kk)
        call codent(kk, 'D0', knumat)
!
!  LE NOM DU CODI EST CELUI DU PREMIER MATERIAU DU GROUPE KK
!
        codi(1:8)=zk8(igrp+icompt)
        codi(9:13)='.'//knumat
        call jeveuo(codi//'.CODI', 'L', zi(jvale+kk-1))
        icompt=icompt+nbmat
50  end do
!
!
90  continue
    chmacz=chmace
    call jedema()
!
end subroutine
