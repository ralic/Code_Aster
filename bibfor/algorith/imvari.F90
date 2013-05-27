subroutine imvari(moclef, iocc, ncomel, lcomel, comcod,&
                  nbvari, tavari)
    implicit none
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
!-----------------------------------------------------------------------
!     ECRITURE DES NOMS DES VARIABLES INTERNES DANS UNE TABLE
! ----------------------------------------------------------------------
! OUT COMCOD  : OBJET DECRIVANT LE TYPE DE COMPORTEMENT (ISSU DE LCCREE)
! OUT CARCRI  : OBJET CARCRI(9) CRITERES DE CONVERGENCE LOCAUX
! OUT NBVARI  : NOMBRE DE VARIABLE INTERNES
! OUT k       : =1 si COMP_INCR, =2 si COMP_ELAS
    include 'jeveux.h'
!
    include 'asterc/lccree.h'
    include 'asterc/lcinfo.h'
    include 'asterc/lcvari.h'
    include 'asterfort/indk16.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbvari, ncomel, iocc, ii, nums(2), lvari, icom, icomm
    integer :: nbvarm, nbvarl, numlc, numv, lvar2, indi
    character(len=16) :: comcod, lcomel(5), noms(2), moclef, comcom
    character(len=8) :: tavari
!-----------------------------------------------------------------------
!
    call jemarq()
! exceptions
    if ((lcomel(1)(5:11).eq.'CRISTAL') .and. (lcomel(2).eq.'SIMO_MIEHE')) then
        call u2mess('I', 'COMPOR2_25')
        call u2mess('I', 'COMPOR2_26')
        elseif ((lcomel(2).eq.'SIMO_MIEHE').and. (lcomel(1)(5:10)&
    .eq.'_ISOT_')) then
        call wkvect('&&PMDORC.LVARI', 'V V K16', nbvari, lvari)
        call wkvect('&&PMDORC.LVAR2', 'V V K16', nbvari, lvar2)
        call lcvari(comcod, nbvari, zk16(lvari))
        noms(1)=moclef
        noms(2)=lcomel(1)
        nums(1)=iocc
        nums(2)=nbvari
        call u2mesg('I', 'COMPOR2_23', 2, noms, 2,&
                    nums, 0, 0.d0)
!        PLACER INDIPLAS  A LA FIN
        indi = indk16( zk16(lvari), 'INDIPLAS', 1, nbvari )
        do 561 ii = 1, indi-1
            zk16(lvar2-1+ii)=zk16(lvari-1+ii)
561      continue
        do 562 ii = indi, nbvari-1
            zk16(lvar2-1+ii)=zk16(lvari-1+ii+1)
562      continue
        zk16(lvar2-1+nbvari)=zk16(lvari-1+indi)
        do 563 ii = 1, nbvari
            call u2mesg('I', 'COMPOR2_24', 1, zk16(lvar2-1+ii), 1,&
                        ii, 0, 0.d0)
563      continue
        call jedetr('&&PMDORC.LVAR2')
        call jedetr('&&PMDORC.LVARI')
!
        elseif ((lcomel(2).eq.'SIMO_MIEHE').and. (lcomel(1)(1:4)&
    .eq.'META')) then
! A FAIRE PLUS TARD
!
        elseif ((lcomel(1)(1:5).eq.'KIT_H').or. (lcomel(1)(1:5)&
    .eq.'KIT_T')) then
!
        call wkvect('&&PMDORC.LVARI', 'V V K16', nbvari, lvari)
        noms(1)=moclef
        noms(2)=lcomel(1)
        nums(1)=iocc
        nums(2)=nbvari
        call u2mesg('I', 'COMPOR2_23', 2, noms, 2,&
                    nums, 0, 0.d0)
        icomm=0
        do 555 icom = 1, ncomel
!           RECHERCHE DU COMPORTEMENT MECANIQUE
            if ((lcomel(icom)(1:3).ne.'HYD') .and. (lcomel(icom)(1:3) .ne.'LIQ') .and.&
                (lcomel(icom)(1:3).ne.'GAZ')) then
                icomm=icom
            endif
555      continue
        numv=0
        call lccree(1, lcomel(icomm), comcom)
        call lcinfo(comcom, numlc, nbvarm)
        call lcvari(comcom, nbvarm, zk16(lvari))
        do 556 ii = 1, nbvarm
            call u2mesg('I', 'COMPOR2_24', 1, zk16(lvari-1+ii), 1,&
                        ii, 0, 0.d0)
556      continue
!
        numv=nbvarm
        do 558 icom = 1, ncomel
!           RECHERCHE DU COMPORTEMENT MECANIQUE
            if (icom .eq. icomm) goto 558
            call lccree(1, lcomel(icom), comcom)
            call lcinfo(comcom, numlc, nbvarl)
            call lcvari(comcom, nbvarl, zk16(lvari))
            do 557 ii = 1, nbvarl
                numv=numv+ii
                call u2mesg('I', 'COMPOR2_24', 1, zk16(lvari-1+ii), 1,&
                            numv, 0, 0.d0)
557          continue
558      continue
!
        call jedetr('&&PMDORC.LVARI')
!
!
    else if (nbvari.gt.0) then
!
        call wkvect('&&PMDORC.LVARI', 'V V K16', nbvari, lvari)
        call lcvari(comcod, nbvari, zk16(lvari))
        noms(1)=moclef
        noms(2)=lcomel(1)
        nums(1)=iocc
        nums(2)=nbvari
        call u2mesg('I', 'COMPOR2_23', 2, noms, 2,&
                    nums, 0, 0.d0)
!
        do 554 ii = 1, nbvari
            call u2mesg('I', 'COMPOR2_24', 1, zk16(lvari-1+ii), 1,&
                        ii, 0, 0.d0)
554      continue
!
        call jedetr('&&PMDORC.LVARI')
!
    endif
!
    call jedema()
!
end subroutine
