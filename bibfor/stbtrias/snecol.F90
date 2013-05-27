subroutine snecol(imod, nbnode)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
!     =================
!A PRESUPER
!
!     ================================================================
!     !                                                              !
!     !  FONCTION: ECRITURE DES GROUPES DE NOEUDS ASSOCIES           !
!     !            AUX COULEURS                                      !
!     !                                                              !
!     ================================================================
!     !                                                              !
!     !  ROUTINES APPELES : CODENT                                   !
!     !                          : IUNIFI (FONCTION)                 !
!     !                          : CODNOP                            !
!     !                                                              !
!     !  ROUTINE APPELANTE : PRESUP                                  !
!     !                                                              !
!     ================================================================
!
!
    include 'jeveux.h'
!
    include 'asterfort/codent.h'
    include 'asterfort/codnop.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=1) :: prfnoe
    character(len=4) :: kbid
    character(len=8) :: chnode, chgrou
    logical :: logiq(256)
    integer :: jpo(256), jnomb(256), jmax(256)
!  ------------ FIN DECLARATION -------------
!
!  -->N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
!-----------------------------------------------------------------------
    integer :: i, ic, icmax, icol, imod, inum, ipos
    integer :: j, jinfo, nbmax, nbno, nbnode, nbtot
!-----------------------------------------------------------------------
    call jemarq()
!
!
    prfnoe='N'
    icmax = 256
    do 10 i = 1, icmax
        logiq(i) = .false.
        jpo(i) = 0
        jnomb(i) = 0
        jmax(i) = 1000
10  end do
!
    nbmax = 1000
    call jeveuo('&&PRESUP.INFO.NOEUDS', 'L', jinfo)
    do 100 i = 1, nbnode
        inum = zi(jinfo-1+(i-1)*3+1)
        call codnop(chnode, prfnoe, 1, 1)
        call codent(inum, 'G', chnode(2:8))
        icol = zi(jinfo-1+(i-1)*3+3)
        ipos = icol + 1
        if (ipos .gt. icmax) then
            call u2mess('A', 'STBTRIAS_2')
            goto 100
        endif
        if (.not.logiq(ipos)) then
            logiq(ipos)= .true.
            call codent(icol, 'G', kbid)
            call wkvect('&&PRESUP.COUL'//kbid, 'V V K8', nbmax+1, jpo( ipos))
        endif
        nbno = jnomb(ipos)
        nbtot= jmax(ipos)
        if (nbno .ge. nbtot) then
            call codent(icol, 'G', kbid)
            nbtot = nbtot + nbmax
            jmax(ipos) = nbtot
            call juveca('&&PRESUP.COUL'//kbid, nbtot+1)
            call jeveuo('&&PRESUP.COUL'//kbid, 'E', jpo(ipos))
        endif
        jnomb(ipos) = nbno + 1
        zk8(jpo(ipos)-1+nbno+1) = chnode
100  end do
!
! --> ECRITURE DES GROUPES DE NOEUDS PAR COULEUR
!
    do 110 ic = 1, icmax
        if (logiq(ic)) then
            call codent((ic-1), 'G', kbid)
            chgrou = 'COUL_'//kbid
            write(imod,'(A,4X,2A)')'GROUP_NO','NOM=',chgrou
            nbno = jnomb(ic)
            write (imod,'(8(2X,A))') (zk8(jpo(ic)-1+j),j=1,nbno)
            write (imod,'(A)') 'FINSF'
            write (imod,'(A)') '%'
            call jedetr('&&PRESUP.COUL'//kbid)
        endif
110  end do
!
    call jedema()
end subroutine
