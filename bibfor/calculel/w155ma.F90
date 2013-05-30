subroutine w155ma(numa, nucou, nicou, nangl, nufib,&
                  motfac, jce2d, jce2l, jce2v, jce5d,&
                  jce5l, jce5v, ksp1, ksp2, c1,&
                  c2, iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
    implicit none
    include 'jeveux.h'
    include 'asterc/r8pi.h'
    include 'asterfort/assert.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/u2mess.h'
    integer :: numa, nucou, nangl, nufib, ksp1, ksp2
    integer :: jce2l, jce2d, jce2v, iret, jce5l, jce5d, jce5v
    real(kind=8) :: c1, c2
    character(len=3) :: nicou
    character(len=16) :: motfac
    integer :: iposi, nbcou, nbsec, isect, icou, npgh
    integer :: iad2, nbfib, iad2a
    real(kind=8) :: poi(2), omega, pi, angle, dxa
!
! ----------------------------------------------------------------------
! BUT : DETERMINER KSP1, KSP2, C1 ET C2
!       TELS QUE POUR LA MAILLE NUMA, LE SOUS-POINT SPECIFIE PAR
!       (MOTFAC,NUCOU,NICOU,NANGL,NUFIB)
!       SOIT = C1*KSP1+C2*KSP2
! OUT IRET : / 0 -> OK : ON PEUT UTILISER C1,C2,KSP1 ET KSP2
!            / 1 -> LA MAILLE NUMA N'EST PAS CONCERNEE
!
! REMARQUE : L'INTERPOLATION N'EST NECESSAIRE QUE POUR LES TUYAUX.
!            DANS LES AUTRES CAS, KSP2=KSP1, C2=0., C1=1.
! ----------------------------------------------------------------------
    iret=0
!
!
!
!     -- CALCUL DE IPOSI=1,2,3 : POSITION DANS LA COUCHE
    if (motfac .eq. 'EXTR_COQUE' .or. motfac .eq. 'EXTR_TUYAU') then
        if (nicou .eq. 'INF') then
            iposi=1
        else if (nicou.eq.'MOY') then
            iposi=2
        else if (nicou.eq.'SUP') then
            iposi=3
        else
            call assert(.false.)
        endif
    endif
!
!
!
!
    if (motfac .eq. 'EXTR_COQUE') then
!     ---------------------------------
        call assert(nucou.ge.1)
!       -- EXTR_COQUE EST UTILISE POUR LES COQUES :
!       -- CMP1 = COQ_NCOU
        call cesexi('C', jce2d, jce2l, numa, 1,&
                    1, 1, iad2a)
!
        if (iad2a .le. 0) then
            iret=1
            goto 9999
        else
            nbcou=zi(jce2v-1+iad2a)
            npgh=3
        endif
        if (nucou .gt. nbcou) call u2mess('F', 'CALCULEL2_14')
!
        ksp1=((nucou-1)*npgh)+iposi
        ksp2=ksp1
        c1=1.d0
        c2=0.d0
!
!
    else if (motfac.eq.'EXTR_PMF') then
!     ---------------------------------
        call assert(nufib.ge.1)
!       -- CMP4 = NBFIBR
        call cesexi('C', jce2d, jce2l, numa, 1,&
                    1, 4, iad2)
        if (iad2 .le. 0) then
            iret=1
            goto 9999
        endif
        nbfib=zi(jce2v-1+iad2)
        if (nufib .gt. nbfib) call u2mess('F', 'CALCULEL2_16')
        ksp1=nufib
        ksp2=ksp1
        c1=1.d0
        c2=0.d0
!
!
    else if (motfac.eq.'EXTR_TUYAU') then
!     ---------------------------------
        call assert(nucou.ge.1)
!       -- CMP2 = TUY_NCOU
        call cesexi('C', jce2d, jce2l, numa, 1,&
                    1, 2, iad2)
        if (iad2 .le. 0) then
            iret=1
            goto 9999
        endif
        nbcou=zi(jce2v-1+iad2)
        if (nucou .gt. nbcou) call u2mess('F', 'CALCULEL2_14')
!       -- CMP3 = TUY_NSEC
        call cesexi('C', jce2d, jce2l, numa, 1,&
                    1, 3, iad2)
        call assert(iad2.gt.0)
        nbsec=zi(jce2v-1+iad2)
        if (nucou .gt. nbcou) call u2mess('F', 'CALCULEL2_14')
        icou=2*(nucou-1)+iposi
!
!       -- CMP1 = ANGZZK
        call cesexi('C', jce5d, jce5l, numa, 1,&
                    1, 1, iad2)
        omega=zr(jce5v-1+iad2)
!
!       -- MORCEAU DE CODE EXTRAIT DE TE0597 :
        pi=r8pi()
        angle=nangl*pi/180.d0
!       CALL CARCOU(...,OMEGA)
        dxa=pi/nbsec
        isect=int((omega+angle)/dxa)+1
        poi(1)=1.d0-((omega+angle)/dxa+1.d0-isect)
        poi(2)=1.d0-(isect-(omega+angle)/dxa)
        if (isect .gt. (2*nbsec)) isect=isect-2*nbsec
        if (isect .lt. 1) isect=isect+2*nbsec
        if (isect .le. 0 .or. isect .gt. (2*nbsec)) call u2mess('F', 'ELEMENTS4_51')
!       -- FIN MORCEAU TE0597
        ksp1=(2*nbsec+1)*(icou-1)+isect
        ksp2=ksp1+1
        c1=poi(1)
        c2=poi(2)
!
!
    else
        call assert(.false.)
    endif
!
9999  continue
!
!
end subroutine
