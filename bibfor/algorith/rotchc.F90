subroutine rotchc(profno, cvale, tetss, nbss, invsk,&
                  nbnot, nbcmp, iax)
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
!    T. KERBER     DATE 16/05/93
!-----------------------------------------------------------------------
!  BUT: EFFECTUER LA ROTATION DE LA PARTIE DES CHAMNO.VALE CORRESPON-
!  -DANT A CHAQUE SOUS-STRUCTURE A PARTIR DU PROFNO GLOBAL, DU
!  MAILLAGE SQUELETTE GLOBAL, ET DU TABLEAU INV-SKELET ET DU VECTEUR
!  DES ANGLES DE ROTATION DE CHAQUE SOUS-STRUCTURE
    implicit none
!
!-----------------------------------------------------------------------
!
! PROFNO   /I/: NOM K19 DU PROF_CHNO GLOBAL
! CVALE    /M/: VECTEUR CORRESPONDANT AU .VALE DU CHAMNO COURANT
! TESSS    /I/: VECTEUR DES ANGLE DE ROTATION DES SOUS-STRUCTURES
! NBSS     /I/: NOMBRE DE SOUS-STRUCTURES
! INVSK    /I/: TABLEAU INVERSE-SKELETTE
! NBNOT    /I/: NOMBRE DE NOEUDS GLOBAL
! NBCMP    /I/: NOMBRE DE COMPOSANTE MAX DE LA GRANDEUR SOUS-JACENTE
! IAX      /I/: NUMERO DE L'AXE DE ROTATION
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/intet0.h'
    include 'asterfort/isdeco.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
!
!
!-----------------------------------------------------------------------
    integer :: i, iax, ibid, icomp, ierd, inueq, j
    integer :: k, llnueq, llprno, ltidec, nbcmp, nbcmpm, nbec
    integer :: nbnot, nbss, numsec
    real(kind=8) :: tetac, tetcou
!-----------------------------------------------------------------------
    parameter(nbcmpm=10)
    character(len=6) :: pgc
    character(len=8) :: kbid, nomg
    character(len=19) :: profno
    character(len=24) :: prno, nueq
    integer :: invsk(nbnot, 2), ieq(nbcmpm)
    real(kind=8) :: tetss(nbss), tet0(nbcmpm, nbcmpm)
    complex(kind=8) :: cvale(*), udep(nbcmpm)
!
!-----------------------------------------------------------------------
    data pgc /'ROTCHC'/
!-----------------------------------------------------------------------
!
!------------------------RECUPERATION DU PRNO DEEQ NUEQ-----------------
!
    call jemarq()
!
!-----RECUPERATION DU NOMBRE DU NOMBRE D'ENTIERS CODES ASSOCIE A DEPL_R
!
    nomg = 'DEPL_R'
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                kbid, ierd)
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    endif
!
    nueq=profno//'.NUEQ'
    prno=profno//'.PRNO'
!
    call jenonu(jexnom(prno(1:19)//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(prno, ibid), 'L', llprno)
    call jeveuo(nueq, 'L', llnueq)
!
!----------------------ALLOCATION VECTEUR DECODAGE----------------------
!
    call wkvect('&&'//pgc//'.DECODAGE', 'V V I', nbcmp, ltidec)
!
!---------------------------ROTATION------------------------------------
!
    tetcou=tetss(1)
    call intet0(tetcou, tet0, iax)
!
    do 10 i = 1, nbnot
!
        numsec=invsk(i,1)
        tetac=tetss(numsec)
        if (tetac .ne. tetcou) then
            tetcou=tetac
            call intet0(tetcou, tet0, iax)
        endif
!
        inueq=zi(llprno+(nbec+2)*(i-1))
        call isdeco(zi(llprno+(nbec+2)*(i-1)+2), zi(ltidec), nbcmp)
        icomp=0
!
        do 20 j = 1, nbcmpm
            if (zi(ltidec+j-1) .gt. 0) then
                icomp=icomp+1
                ieq(j)=zi(llnueq+inueq+icomp-2)
                udep(j)=cvale(ieq(j))
            else
                ieq(j)=0
                udep(j)=0.d0
            endif
20      continue
!
        do 30 j = 1, nbcmpm
            if (ieq(j) .gt. 0) then
                cvale(ieq(j))=0.d0
                do 40 k = 1, nbcmpm
                    cvale(ieq(j))=cvale(ieq(j))+tet0(j,k)*udep(k)
40              continue
            endif
30      continue
!
10  end do
!
    call jedetr('&&'//pgc//'.DECODAGE')
!
    call jedema()
end subroutine
