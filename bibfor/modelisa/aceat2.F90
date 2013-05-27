subroutine aceat2(nbtuy, eltuy, notuy, nbpart, noex1,&
                  noex2, nbmap, elpar, nopar, nno)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    integer :: nbtuy, eltuy(nbtuy), notuy(nno*nbtuy), nbpart, noex1(nbpart)
    integer :: noex2(nbpart), nbmap(nbpart), elpar(nbpart, nbtuy)
    integer :: nopar(nbpart, nno, nbtuy)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES TUYAUX
! ----------------------------------------------------------------------
! IN  :
! ----------------------------------------------------------------------
!
!     STOCKAGE DES NUMEROS DE NOEUDS EXTREMITES
!
!-----------------------------------------------------------------------
    integer :: iext1, iext2, im1, ima, ipa, jma, kp
    integer :: nbe, nbext1, nbext2, nex1, ni1, ni2, ni3
    integer :: ni4, nj1, nj2, nj3, nj4, nno
!-----------------------------------------------------------------------
    nbext1=0
    nbext2=0
    do 30 ima = 1, nbtuy
        iext1=0
        iext2=0
!JMP         NI1 = NOTUY(3*IMA-2)
!JMP         NI2 = NOTUY(3*IMA-1)
        ni1 = notuy(nno*(ima-1)+1)
        ni2 = notuy(nno*(ima-1)+2)
        do 40 jma = 1, nbtuy
            if (jma .ne. ima) then
!JMP               NJ1 = NOTUY(NNO*JMA-2)
!JMP               NJ2 = NOTUY(NNO*JMA-1)
                nj1 = notuy(nno*(jma-1)+1)
                nj2 = notuy(nno*(jma-1)+2)
                if (ni1 .eq. nj2) then
                    iext1=1
                endif
                if (ni2 .eq. nj1) then
                    iext2=1
                endif
            endif
40      continue
        if (iext1 .eq. 0) then
            nbext1=nbext1+1
            noex1(nbext1)=ni1
        endif
        if (iext2 .eq. 0) then
            nbext2=nbext2+1
            noex2(nbext2)=ni2
        endif
30  end do
    call assert(nbext1.eq.nbext2)
    call assert(nbext1.eq.nbpart)
!
! --- VERIFICATION ET STOCKAGE DES PARTIES CONNEXES
!     HYPOTHESE : LES MAILLES SONT TOUTES ORIENTEES DANS LE MEME SENS
!
    im1=0
    do 10 ipa = 1, nbpart
        nex1=noex1(ipa)
! RECHERCHE DE LA PREMIERE MAILLE
        do 20 ima = 1, nbtuy
!JMP            NI1 = NOTUY(NNO*IMA-2)
!JMP            NI2 = NOTUY(NNO*IMA-1)
!JMP            NI3 = NOTUY(NNO*IMA)
            ni1 = notuy(nno*(ima-1)+1)
            ni2 = notuy(nno*(ima-1)+2)
            ni3 = notuy(nno*(ima-1)+3)
            if (nno .eq. 4) then
                ni4 = notuy(nno*(ima-1)+4)
            endif
            if (nex1 .eq. ni1) then
                nbe = 1
                elpar(ipa,nbe)=eltuy(ima)
                nopar(ipa,1,nbe)=ni1
                nopar(ipa,2,nbe)=ni2
                nopar(ipa,3,nbe)=ni3
                if (nno .eq. 4) then
                    nopar(ipa,4,nbe)=ni4
                endif
                goto 21
            endif
20      continue
21      continue
        im1=ima
41      continue
! SI NI2 EST UNE EXTREMIE, ON CHANGE DE PARTIE
!JMP         NI2 = NOTUY(3*IM1-1)
        ni2 = notuy(nno*(im1-1)+2)
        do 50 kp = 1, nbpart
            if (ni2 .eq. noex2(kp)) goto 11
50      continue
! RECHERCHE DE LA MAILLE ATTENANTE A IM1
        do 42 jma = 1, nbtuy
            if (im1 .eq. jma) goto 42
!JMP            NJ1 = NOTUY(3*JMA-2)
!JMP            NJ2 = NOTUY(3*JMA-1)
!JMP            NJ3 = NOTUY(3*JMA)
            nj1 = notuy(nno*(jma-1)+1)
            nj2 = notuy(nno*(jma-1)+2)
            nj3 = notuy(nno*(jma-1)+3)
            if (nno .eq. 4) then
                nj4 = notuy(nno*(jma-1)+4)
            endif
            if (ni2 .eq. nj1) then
                nbe = nbe+1
                elpar(ipa,nbe)=eltuy(jma)
                nopar(ipa,1,nbe)=nj1
                nopar(ipa,2,nbe)=nj2
                nopar(ipa,3,nbe)=nj3
                if (nno .eq. 4) then
                    nopar(ipa,4,nbe)=nj4
                endif
                goto 43
            endif
42      continue
43      continue
        im1=jma
        goto 41
11      continue
        nbmap(ipa)=nbe
10  end do
end subroutine
