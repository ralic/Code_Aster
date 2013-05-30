subroutine calcin(option, max, may, maz, model,&
                  veprj, modx, mody, modz, i,&
                  j, mij)
!-------------------------------------------------------------------
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
!-------------------------------------------------------------------
    implicit none
!
! ROUTINE CALCULANT LA MASSE AJOUTEE SUR LE MODELE THERMIQUE
!  D INTERFACE AINSI QUE LE PREMIER COEFFICIENT D AMORTISSEMENT
!  AJOUTE
! IN :MAX,MAY,MAZ : MATRICES AX ET AY ET AZ
! IN: VEPRJ: PRESSION PROJETEE DUE AU MODE OU CHAMNO J
! IN: MODEL: K2 : CHAINE DISTINGUANT LE TYPE DE MODELISATION
! IN: MODX,MODY,MODZ : DEPLACEMENTS PROJETES
! OUT : MIJ : MASSE AJOUTEE
!-------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mrmult.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    integer :: ipres, i, j
    real(kind=8) :: mij
    character(len=*) :: model, option
    character(len=19) :: modx, mody, modz, veprj, max, may, maz
    character(len=1) :: k1bid
!--------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: imatx, imaty, imatz, imodx, imody, imodz, ivecx
    integer :: ivecy, ivecz, nbpres
    real(kind=8) :: rx, ry, rz
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(modx//'.VALE', 'L', imodx)
    call jeveuo(mody//'.VALE', 'L', imody)
!
    call jeveuo(veprj//'.VALE', 'L', ipres)
    call jelira(veprj//'.VALE', 'LONMAX', nbpres, k1bid)
!
    call wkvect('&&CALCIN.VECTX', 'V V R', nbpres, ivecx)
    call wkvect('&&CALCIN.VECTY', 'V V R', nbpres, ivecy)
!
! --- RECUPERATION DES DESCRIPTEURS DE MATRICES ASSEMBLEES MAX ET MAY
!
    call mtdscr(max)
    call jeveuo(max(1:19)//'.&INT', 'E', imatx)
    call mtdscr(may)
    call jeveuo(may(1:19)//'.&INT', 'E', imaty)
!
!------MULTIPLICATIONS MATRICE MAX * CHAMNO MODX---------------------
!----------ET MATRICE MAY * CHAMNO MODY------------------------------
!
    call mrmult('ZERO', imatx, zr(imodx), zr(ivecx), 1,&
                .true.)
    call mrmult('ZERO', imaty, zr(imody), zr(ivecy), 1,&
                .true.)
!
!--PRODUITS SCALAIRES VECTEURS PRESSION PAR MAX*MODX ET MAY*MODY
!
    rx= ddot(nbpres,zr(ipres), 1,zr(ivecx),1)
    ry= ddot(nbpres,zr(ipres), 1,zr(ivecy),1)
!
!
!---------------- MENAGE SUR LA VOLATILE ---------------------------
!
!
    call jedetr('&&CALCIN.VECTX')
    call jedetr('&&CALCIN.VECTY')
!
    call detrsd('CHAM_NO', modx)
    call detrsd('CHAM_NO', mody)
!
!
!
!
!
!------ MASSE AJOUTEE = PRESSION*MAX*MODX + PRESSION*MAY*MODY-------
!--------------------------+ PRESSION*MAZ*MODZ  EN 3D---------------
    if (model .eq. '3D') then
!
        call jeveuo(modz//'.VALE', 'L', imodz)
        call wkvect('&&CALCIN.VECTZ', 'V V R', nbpres, ivecz)
        call mtdscr(maz)
        call jeveuo(maz(1:19)//'.&INT', 'E', imatz)
        call mrmult('ZERO', imatz, zr(imodz), zr(ivecz), 1,&
                    .true.)
        rz= ddot(nbpres,zr(ipres), 1,zr(ivecz),1)
        call jedetr('&&CALCIN.VECTZ')
        call detrsd('CHAM_NO', modz)
        mij = rx+ry+rz
!
    else
        mij = rx+ry
    endif
!
    if ((i.eq.j) .and. (mij.lt.0) .and. (option.eq.'MASS_AJOU')) then
        call u2mess('A', 'ALGORITH_60')
    endif
    if ((i.eq.j) .and. (mij.lt.0) .and. (option.eq.'AMOR_AJOU')) then
        call u2mess('A', 'ALGORITH_61')
    endif
!
    call detrsd('CHAM_NO', veprj)
!
!-----------------------------------------------------------------
    call jedema()
end subroutine
