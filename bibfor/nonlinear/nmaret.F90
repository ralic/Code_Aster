subroutine nmaret(nbarvz, nno, ndim, nliseq, nbno,&
                  numnod, gro1, gro2)
!
! aslint: disable=W1306
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/xrell2.h'
    integer :: nbarvz, nno, ndim, nbno
    character(len=19) :: nliseq
    character(len=24) :: gro1, gro2, numnod
!
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
!
! ----------------------------------------------------------------------
!
! INITIALISATION DU PILOTAGE DDL_IMPO OU LONG_ARC - FORMULATION XFEM
!
! RENVOIE L'ENSEMBLE DES ARETES PILOTEES A PARTIR DES NOEUDS ENTRES PAR
! L'UTILISATEUR ET D'UNE LISTE D'ARETES VITALES
! SI L'UTILISATEUR N'A RIEN ENTRE, RENVOIE UN ENSEMBLE D'ARETES
! INDEPENDANTES
!
! ----------------------------------------------------------------------
!
!
! IN  NBARVI : NOMBRE D ARETES VITALES
! IN  NNO    : NOMBRE DE NOEUDS ENTRES PAR L'UTILISATEUR (EVT 0)
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NLISEQ : LISTE DES RELATIONS D EGALITE
! IN  NUMNOD : LISTE DES NUMEROS DES NOEUDS UTILISATEUR
! OUT  NBNO  : NOMBRE D ARETES FINALEMENT PILOTEES
! OUT  GRO1  : LISTE DES NOEUDS EXTREMITES 1 DES ARETES PILOTEES
! OUT  GRO1  : LISTE DES NOEUDS EXTREMITES 2 DES ARETES PILOTEES
!
!
!
!
    integer :: tabnoz(3, nbarvz), tabut(3, nbarvz), nbarvi
    real(kind=8) :: tabcoz(ndim, nbarvz), tabcrz(nbarvz)
    integer :: jgro1, jgro2, j, jgro, i, l, nreleq, repere, jlis1
    integer :: effac, nar, narm, iret, vali(2)
    logical :: noeuad
    character(len=8) :: k8bid
    character(len=19) :: nlisco, nlisrl, nlise2
!
! ----------------------------------------------------------------------
!
    nbarvi=nbarvz
    call jeveuo(nliseq, 'L', jlis1)
    call jeexin(numnod, iret)
    if (nno .ge. 1 .and. iret .ne. 0) call jeveuo(numnod, 'L', jgro)
    do 10 i = 1, nbarvi
        tabnoz(1,i)=zi(jlis1-1+2*(i-1)+1)
        tabnoz(2,i)=zi(jlis1-1+2*(i-1)+2)
        tabnoz(3,i)=i
        do 11 j = 1, ndim
            tabcoz(j,i)=0.d0
11      continue
        tabcrz(i)=i
10  end do
!
!
    if (nno .ge. 1 .and. iret .ne. 0) then
        do 20 i = 1, nno
            tabut(1,i)=zi(jgro-1+i)
20      continue
        if (nno .gt. nbarvi) call u2mess('F', 'PILOTAGE_62')
        do 40 j = 1, nno
            noeuad=.false.
            do 41 i = 1, nbarvi
!
                if (tabut(1,j) .eq. tabnoz(1,i)) then
                    do 42 l = 1, nno
                        if ((l.ne.j) .and. (tabut(1,l).eq.tabnoz(2,i))) then
                            vali(1)=tabut(1,j)
                            vali(2)=tabut(1,l)
                            call u2mesi('F', 'PILOTAGE_63', 2, vali)
                        endif
42                  continue
                    tabut(2,j)=tabnoz(2,i)
                    tabut(3,j)=tabnoz(3,i)
                    noeuad=.true.
                else if (tabut(1,j).eq.tabnoz(2,i)) then
                    do 43 l = 1, nno
                        if ((l.ne.j) .and. (tabut(1,l).eq.tabnoz(1,i))) then
                            vali(1)=tabut(1,j)
                            vali(2)=tabut(1,l)
                            call u2mesi('F', 'PILOTAGE_63', 2, vali)
                        endif
43                  continue
                    tabut(2,j)=tabnoz(1,i)
                    tabut(3,j)=tabnoz(3,i)
                    noeuad=.true.
                endif
41          continue
            if (.not.noeuad) call u2mesi('A', 'PILOTAGE_61', 1, tabut(1, j))
40      continue
        do 50 i = 1, nno
            do 51 j = 1, 3
                tabnoz(j,i)=tabut(j,i)
51          continue
50      continue
        nbarvi=nno
    endif
!
    nlise2 = '&&NMARET.LISEQ'
    nlisrl = '&&NMARET.LISRL'
    nlisco = '&&NMARET.LISCO'
    call xrell2(tabnoz, ndim, nbarvi, tabcoz, tabcrz,&
                .true., nlise2)
    nar=nbarvi
    call jeexin(nlise2, iret)
    if (iret .ne. 0) then
        call jeveuo(nlise2, 'L', jlis1)
        call jelira(nlise2, 'LONMAX', nreleq, k8bid)
        nreleq = nreleq/2
        call assert(nreleq.eq.nar)
        if (nreleq .gt. 0) then
            do 60 i = 1, nreleq
                repere=0
                effac=zi(jlis1-1+2*(i-1)+2)
                do 61 j = 1, nar
                    if (effac .eq. tabnoz(3,j)) then
                        repere=j
                    endif
61              continue
                if (repere .gt. 0) then
                    if (repere .lt. nar) then
                        narm = nar-1
                        do 220 l = repere, narm
                            tabnoz(1,l) = tabnoz(1,l+1)
                            tabnoz(2,l) = tabnoz(2,l+1)
                            tabnoz(3,l) = tabnoz(3,l+1)
220                      continue
                    endif
                    tabnoz(1,nar)=0
                    tabnoz(2,nar)=0
                    tabnoz(3,nar)=0
                    nar=nar-1
                endif
60          continue
        endif
    endif
    nbno = nar
    call wkvect(gro1, 'V V I', nbno, jgro1)
    call wkvect(gro2, 'V V I', nbno, jgro2)
!
    do 30 i = 1, nbno
        zi(jgro1-1+i)=tabnoz(1,i)
        zi(jgro2-1+i)=tabnoz(2,i)
30  end do
    call jedetr(nlise2)
    call jedetr(nlisrl)
    call jedetr(nlisco)
end subroutine
