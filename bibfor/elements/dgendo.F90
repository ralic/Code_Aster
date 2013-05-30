subroutine dgendo(em, ef, h, syt, syc,&
                  num, nuf, pendt, pelast, pendf,&
                  pelasf, iendo, icisai, icompr, gt,&
                  gf, gc, ipente, np, dxp)
!
    implicit   none
!
! PARAMETRES ENTRANTS
    include 'asterfort/u2mesk.h'
    include 'asterfort/zerop3.h'
    integer :: iendo, icompr, icisai, ipente
    real(kind=8) :: em, ef, h, syt, syc, num, nuf, np, dxp
    real(kind=8) :: pendt, pendc, pelast, pendf, pelasf
!
! PARAMETRES SORTANTS
    real(kind=8) :: gt, gf, gc
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! RESONSABLE
! ----------------------------------------------------------------------
!
! BUT : CALCUL DES PARAMETRES D'ENDOMMAGEMENT GAMMA_T, GAMMA_C
!       ET GAMMA_F
!
! IN:
!       EM    : MODULE D YOUNG EN MEMBRANE
!       EF    : MODULE D YOUNG EN FLEXION
!       H     : EPAISSEUR DE LA PLAQUE
!       SYT   : SEUIL D'ENDOMMAGEMENT EN TRACTION
!       SYC   : SEUIL D'ENDOMMAGEMENT EN COMPRESSION
!       NUM   : COEFF DE POISSON EN MEMBRANE
!       NUF   : COEFF DE POISSON EN FLEXION
!       PENDT : PENTE POST ENDOMMAGEMENT EN MEMBRANNE
!       PELAST: PENTE ELASTIQUE EN TRACTION
!       PENDF : PENTE POST ENDOMMAGEMENT EN FLEXION
!       PELASF: PENTE ELASTIQUE EN FLEXION
!       IENDO :
!       ICISAI: INDICATEUR DE CISAILLEMENT
!       ICOMPR: METHODE DE COMPRESSION
!       IPENTE: OPTION DE CALCUL DES PENTES POST ENDOMMAGEMENT
!               1 : RIGI_ACIER
!               2 : PLAS_ACIER
!               3 : UTIL
!       NP    : EFFORT A PLASTICITE
!       DXP   : DEPLACEMENT A PLASTICITE
! OUT:
!       GT    : GAMMA DE TRACTION
!       GF    : GAMMA DE FLEXION
!       GC    : GAMMA DE CISAILLEMENT
! ----------------------------------------------------------------------
! PARAMETRES INTERMEDIAIRES
    integer :: ns, ii
    real(kind=8) :: c1, c2, c3, alpha, beta, x(3), gt3(3)
    real(kind=8) :: xf, gf1, gf2, sytxy, dxd
!
! DETERMINATION DE GAMMA_T ET DE GAMMA_C PAR ESSAI DE TRACTION
! METHODE 1
    if (iendo .eq. 1) then
        if (icompr .eq. 1) then
            gt=(-16.d0*pendt*syc**4*num**3+9.d0*pendt*num**8*syc**4+&
            4.d0* pendt*syc**4*num-2.d0*pendt*syc**4*num**2+6.d0*&
            pendt*syc**4*num**4+2.d0*pendt*syt**4*num**4+26.d0*pendt*&
            syc**4*num**5+6.d0*pendt* syt**4*num**5-14.d0*pendt*num**&
            6*syc**4+pendt*num**6*syt**4-14.d0*pendt*num**7*syc**4-&
            2.d0*pendt*num**7*syt**4-pendt*syt**4*num**2- 2.d0*pendt*&
            syt**4*num**3-em*h*syc**2*syt**2*num**2+em*h*syc**2*&
            num**3*syt**2+7.d0*em*h*syc**2*num**4*syt**2+em*h*syc**2*&
            num**5 *syt**2-5.d0*em*h*num**6*syc**2*syt**2-em*h*num**7*&
            syc**2*syt**2 +pendt*syc**2*syt**2*num**2-pendt*syc**2*&
            num**3*syt**2-7.d0* pendt*syc**2*num**4*syt**2-pendt*syc**&
            2*num**5*syt**2+5.d0*pendt *num**6*syc**2*syt**2+pendt*&
            num**7*syc**2*syt**2+pendt*syc**4- 4.d0*em*h*syc**4*num**&
            3-7.d0*em*h*syc**4*num**4-2.d0*em*h*syt**4* num**4+10.d0*&
            em*h*syc**4*num**5-6.d0*em*h*syt**4*num**5+12.d0*em* h*&
            num**6*syc**4-em*h*num**6*syt**4-10.d0*em*h*num**7*syc**4+&
            2.d0*em*h*num**7*syt**4+em*h*syt**4*num**2+2.d0*em*h*syt**&
            4*num**3)
            gt=gt/(4.d0* em*h*syc**4*num+em*h*syc**4-2.d0*pendt*syc**&
            4*num**3+9.d0*pendt* num**8*syc**4-2.d0*pendt*syc**4*num**&
            2+12.d0*pendt*syc**4*num**4+ 2.d0*pendt*syt**4*num**4+&
            10.d0*pendt*syc**4*num**5+6.d0*pendt* syt**4*num**5-19.d0*&
            pendt*num**6*syc**4+pendt*num**6*syt**4-8.d0* pendt*num**&
            7*syc**4-2.d0*pendt*num**7*syt**4-pendt*syt**4*num**2-&
            2.d0*pendt*syt**4*num**3-2.d0*em*h*syc**2*syt**2*num**2-&
            2.d0*em*h*syc**2*num**3*syt**2+8.d0*em*h*syc**2*num**4*&
            syt**2+8.d0*em*h* syc**2*num**5*syt**2-4.d0*em*h*num**6*&
            syc**2*syt**2-4.d0*em*h* num**7*syc**2*syt**2+2.d0*pendt*&
            syc**2*syt**2*num**2+2.d0*pendt* syc**2*num**3*syt**2-&
            8.d0*pendt*syc**2*num**4*syt**2-8.d0*pendt* syc**2*num**5*&
            syt**2+4.d0*pendt*num**6*syc**2*syt**2+4.d0*pendt*&
            num**7*syc**2*syt**2-18.d0*em*h*syc**4*num**3-13.d0*em*h*&
            syc**4* num**4-2.d0*em*h*syt**4*num**4+26.d0*em*h*syc**4*&
            num**5-6.d0*em*h*syt**4*num**5+17.d0*em*h*num**6*syc**4-&
            em*h*num**6*syt**4-16.d0* em*h*num**7*syc**4+2.d0*em*h*&
            num**7*syt**4+em*h*syt**4*num**2+ 2.d0*em*h*syt**4*num**3)
            gc=1.d0-(1.d0-gt)*(syt**2*(1.d0-num)*(1.d0+2.d0*num)-syc**&
            2* num**2)/(syc**2*(1.d0-num)*(1.d0+2.d0*num)-syt**2*num**&
            2)
        else if (icompr .eq. 2) then
!            GT1=1.D0/2.D0*(PENDT-4.D0*EM*H*NUM**3*GC+EM*H+2.D0*PENDT*
!    &NUM-3.D0*PENDT*
!    &NUM**2+2.D0*EM*H*NUM-EM*H*NUM**2+4.D0*PENDT*NUM**4-2.D0*PENDT*
!    &NUM**2*GC+4.D0*PENDT*NUM**4*GC-6.D0*EM*H*NUM**3+2.D0*PENDT*NUM**3*
!    &GC+sqrt(EM**2*H**2+PENDT**2-8.D0*PENDT*EM*H*NUM**3*GC+4.D0*PENDT*
!    &EM*H*NUM**2*GC**2-24.D0*EM*H*NUM**5*GC**2*PENDT-28.D0*EM*H*NUM**6*
!    &GC**2*PENDT-8.D0*EM*H*NUM**7*GC**2*PENDT-24.D0*EM*H*NUM**4*GC*
!    &PENDT+8.D0*EM*H*NUM**5*GC*PENDT+40.D0*EM*H*NUM**6*GC*PENDT+16.D0*
!    &EM*H*NUM**7*GC*PENDT-24.D0*EM**2*H**2*NUM**6*GC+16.D0*PENDT*EM*H*
!    &NUM**3+4.D0*PENDT**2*NUM+6.D0*PENDT**2*NUM**2-4.D0*PENDT**2*NUM**3
!    &-23.D0*PENDT**2*NUM**4-4.D0*PENDT**2*NUM**2*GC-4.D0*PENDT**2*
!    &NUM**4*GC**2+24.D0*EM**2*H**2*NUM**5*GC**2-4.D0*PENDT**2*NUM**3*
!    &GC+16.D0*EM**2*H**2*NUM**6*GC**2+20.D0*PENDT**2*NUM**4*GC-16.D0*
!    &PENDT**2*NUM**5-16.D0*PENDT**2*NUM**6*GC+12.D0*PENDT**2*NUM**6*
!    &GC**2+8.D0*EM**2*H**2*NUM**5-16.D0*PENDT**2*NUM**7*GC+20.D0*
!    &PENDT**2*NUM**5*GC-12.D0*EM**2*H**2*NUM**3+4.D0*EM**2*H**2*
!    &NUM**2*GC+12.D0*EM**2*H**2*NUM**3*GC-4.D0*EM**2*H**2*NUM**2*
!    &GC**2-8.D0*EM*H*PENDT*NUM-8.D0*EM*H*PENDT*NUM**2+38.D0*EM*H*PENDT*
!    &NUM**4+8.D0*PENDT**2*NUM**6+8.D0*PENDT*NUM**3*EM*H*GC**2+8.D0*
!    &PENDT*NUM**5*EM*H+4.D0*EM**2*H**2*NUM**4*GC-8.D0*EM**2*H**2*NUM**3
!    &*GC**2-2.D0*EM*H*PENDT+12.D0*EM**2*H**2*NUM**6+8.D0*PENDT**2*
!    &NUM**7*GC**2+8.D0*PENDT**2*NUM**7-28.D0*EM**2*H**2*NUM**5*GC+4.D0*
!    &EM**2*H**2*NUM**4*GC**2-20.D0*EM*H*NUM**6*PENDT-8.D0*PENDT*NUM**7*
!    &EM*H-15.D0*EM**2*H**2*NUM**4+4.D0*EM**2*H**2*NUM+2.D0*EM**2*H**2*
!    &NUM**2))/(-6.D0*EM*H*NUM**3+2.D0*EM*H*NUM-EM*H*NUM**2-2.D0*PENDT*
!    &NUM**2+2.D0*PENDT*NUM**3+4.D0*PENDT*NUM**4+EM*H)
            gt=-2.d0*em*h*pendt+4.d0*pendt**2*num+6.d0*pendt**2*num**&
  2         -4.d0*pendt**2*num**3-23.d0*pendt**2*num**4-16.d0*&
            pendt**2*num**5+8.d0*pendt**2*num**6+8.d0* pendt**2*num**&
            7+pendt**2-12.d0*em**2*h**2*num**3-4.d0*pendt**2* num**3*&
            gc+20.d0*pendt**2*num**4*gc+20.d0*pendt**2*num**5*gc+&
            12.d0*pendt**2*num**6*gc**2+8.d0*pendt**2*num**7*gc**2-&
            16.d0*pendt**2* num**6*gc-16.d0*pendt**2*num**7*gc-4.d0*&
            pendt**2*num**4*gc**2- 4.d0*pendt**2*num**2*gc-15.d0*em**&
            2*h**2*num**4+8.d0*em**2*h**2* num**5+12.d0*em**2*h**2*&
            num**6+4.d0*em**2*h**2*num**2*gc
            gt=gt+12.d0* em**2*h**2*num**3*gc-4.d0*em**2*h**2*num**2*&
            gc**2-8.d0*em*h*pendt *num-8.d0*em*h*pendt*num**2+16.d0*&
            em*h*pendt*num**3+38.d0*em*h* pendt*num**4+24.d0*em**2*h**&
            2*num**5*gc**2+16.d0*em**2*h**2* num**6*gc**2-24.d0*em**2*&
            h**2*num**6*gc-8.d0*pendt*num**7*em*h+ 8.d0*pendt*num**5*&
            em*h+4.d0*em**2*h**2*num**4*gc-8.d0*em**2*h**2* num**3*&
            gc**2-28.d0*em**2*h**2*num**5*gc+4.d0*em**2*h**2*num**4*&
            gc**2-20.d0*em*h*num**6*pendt-8.d0*em*h*pendt*num**3*gc+&
            4.d0*em*h*pendt*num**2*gc**2-24.d0*em*h*pendt*num**4*gc+&
            8.d0*pendt*num**3* em*h*gc**2-28.d0*pendt*num**6*gc**2*em*&
            h-8.d0*pendt*num**7*gc**2* em*h+16.d0*pendt*num**7*gc*em*&
            h-24.d0*em*h*num**5*gc**2*pendt+ 8.d0*em*h*num**5*gc*&
            pendt+40.d0*em*h*num**6*gc*pendt+4.d0*em**2* h**2*num+em**&
            2*h**2+2.d0*em**2*h**2*num**2
            gt=0.5d0*(em*h+2.d0*pendt*num+4.d0*pendt*num**4*gc-4.d0*&
            em*h* num**3*gc+4.d0*pendt*num**4-3.d0*pendt*num**2-2.d0*&
            pendt*num**2*gc+2.d0*em*h*num-em*h*num**2-6.d0*em*h*num**&
            3+2.d0*pendt*num**3*gc+ pendt-sqrt(gt))
            gt=gt/(-6.d0*em*h*num**3-em*h*num**2 -2.d0*pendt*num**2+&
            2.d0*pendt*num**3+4.d0*pendt*num**4+em*h+2.d0* em*h*num)
!
!            IF ((GT1 .LT. 1.D0) .AND. (GT1 .GT. 0.D0) .AND.
!     &(GT2 .LT. 1.D0) .AND. (GT2 .GT. 0.D0))THEN
!              GT=MIN(GT1,GT2)
!            ELSE IF ((GT1 .LT. 1.D0) .AND. (GT1 .GT. 0.D0)) THEN
!              GT=GT1
!            ELSE
!              GT=GT2
!            ENDIF
!
            syc=sqrt(-(num-num**2-gt*num**2-gc-gc*num+2.d0*gc *num**2+&
            1.d0)*(num**2-1.d0-num+gt+gt*num-2.d0*gt*num**2+gc*&
            num**2)) *syt/(num-num**2-gt*num**2-gc-gc*num+2.d0*gc*&
            num**2+1.d0)
        endif
! METHODE 2
    else if (iendo .eq. 2) then
        if (icompr .eq. 1) then
            alpha=(syt**2*(1.d0-num)*(1.d0+2.d0*num)-syc**2*num**2)/&
            (syc**2*(1.d0-num)*(1.d0+2.d0*num)-syt**2*num**2)
            beta=em*h/(1.d0+num)/num
            c1=(beta*num-2.d0*beta*alpha+4.d0*beta*alpha*num-pendt*&
            alpha)/(alpha*beta)
            c2=(4.d0*beta*num**2+beta*alpha-beta*num-pendt*num*alpha+&
            pendt*alpha-2.d0*pendt*num-5.d0*beta*alpha*num+5.d0*beta*&
            alpha *num**2)/(beta*alpha)
            c3=(4.d0*beta*num**3-3.d0*beta*alpha*num**2+2.d0*beta*&
            alpha*num**3 +beta*alpha*num-2.d0*pendt*num**2+pendt*num-&
            2.d0*beta*num**2)/ (alpha*beta)
            call zerop3(c1, c2, c3, x, ns)
            do 10 ii = 1, ns
                gt3(ii)=(x(ii)+2.d0*num-1.d0)/num
                if ((gt3(ii) .gt. 0.d0) .and. (gt3(ii) .lt. 1.d0)) then
                    gt=gt3(ii)
                endif
10          continue
            gc=1.d0-(1.d0-gt)*alpha
        else if (icompr .eq. 2) then
            gt=1.d0/2.d0*(num**2*pendt*gc+num**2*pendt+pendt*gc*num+&
            pendt*num-em*h*num-em*h*gc+em*h*gc*num+sqrt(2.d0*num**4*&
            pendt**2*gc+num**4* pendt**2+2.d0*num**3*pendt**2+pendt**&
            2*num**2-2.d0*num**3*pendt*em*h-2.d0*pendt*num**2*em*h+&
            em**2*h**2*num**2+2.d0*num**2*pendt**2* gc+2.d0*num**3*&
            pendt**2*gc**2-2.d0*em**2*h**2*gc**2*num+em**2* h**2*gc**&
            2*num**2+em**2*h**2*gc**2+2.d0*em*h*gc**2*pendt*num-6.d0 *&
            em*h*gc**2*num**3*pendt-2.d0*em*h*num**2*pendt*gc-2.d0*em*&
            h*num* pendt*gc+2.d0*em**2*h**2*num*gc-2.d0*em**2*h**2*&
            num**2*gc+num**4 *pendt**2*gc**2+num**2*pendt**2*gc**2+&
            4.d0*num**3*pendt**2*gc- 4.d0*em*h*gc**2*num**2*pendt))/(&
            em*h*gc*num)
            syc=sqrt(-(num-num**2-gt*num**2-gc-gc*num+2.d0*gc *num**2+&
            1.d0)*(num**2-1.d0-num+gt+gt*num-2.d0*gt*num**2+gc*&
            num**2))*syt/(num-num**2-gt*num**2-gc-gc*num+2.d0*gc*num**&
            2+1.d0)
        endif
!  METHODE 3
    else
        gt=pendt/pelast
        if (icompr .eq. 1) then
            gc=1.d0-(1.d0-gt)*(syt**2*(1.d0-num)*(1.d0+2.d0*num)-syc**&
            2*num**2)/(syc**2*(1.d0-num)*(1.d0+2.d0*num)-syt**2*num**&
            2)
        else if (icompr .eq. 2) then
            syc=sqrt(-(num-num**2-gt*num**2-gc-gc*num+2.d0* gc*num**2+&
            1.d0)*(num**2-1.d0-num+gt+gt*num-2.d0*gt*num**2+gc*&
            num**2))*syt/(num-num**2-gt*num**2-gc-gc*num+2.d0*gc*num**&
  2         +1.d0)
        endif
    endif
!
!
! - PARAMETRES D'ENDOMMAGEMENT MENBRANAIRE EN CISAILLEMENT
!   PUR DANS LE PLAN
    if (icisai .eq. 1) then
! - On calule SYTXY a partir de GT et GC calculé en traction
        sytxy=syt/(1.d0+num)*sqrt(((1.d0-num)*(1.d0+2.d0*num)*&
        (1.d0-gt)+num**2*(1.d0-gc))/(2.d0-gc-gt))
        syt=sytxy
! - PENTE='PENTE_LIM'
        if (ipente .eq. 1) then
            pendc=pendt
! - PENTE ='EPSI_MAX' OU PENTE = 'PLAS'
        else if ((ipente .eq. 3) .or. (ipente .eq. 2)) then
            dxd=sytxy/pelast
            pendc=(np-sytxy)/(dxp-dxd)
        endif
! - METHODE 1
        if (iendo .eq. 1) then
            if (icompr .eq. 1) then
                gt=-(em*h*syc**2+em*h*syc**2*num-3.d0*em*h*syc**2*&
                num**2-3.d0*em*h*syt**2*num**2+em*h*syt**2+em*h*syt**&
                2*num-2.d0*pendc*syc**2- 4.d0*pendc*syc**2*num+2.d0*&
                pendc*syc**2*num**2+4.d0*pendc*syc**2* num**3+2.d0*&
                pendc*syt**2*num**2+2.d0*pendc*syt**2*num**3)/(em*h*&
                (syc**2+syc**2*num-syc**2*num**2+syt**2*num**2-syt**2-&
                syt**2*num))
                gc=1.d0-(1.d0-gt)*(syt**2*(1.d0-num)*(1.d0+2.d0*num)-&
                syc**2* num**2)/(syc**2*(1.d0-num)*(1.d0+2.d0*num)-&
                syt**2*num**2)
            else if (icompr .eq. 2) then
                gt=(-2.d0*em*h+em*h*gc+2.d0*pendc+2.d0*pendc*num)/(em*&
                h)
                syc=sqrt(-(num-num**2-gt*num**2-gc-gc*num+2.d0*gc*&
                num**2+1.d0)*(gc*num**2-num+gt+num**2-1.d0+gt*num-&
                2.d0*gt*num**2))*syt/(num-num**2-gt*num**2-gc-gc*num+&
                2.d0*gc*num**2+1.d0)
            endif
! - METHODE 2
        else if (iendo .eq. 2) then
            if (icompr .eq. 1) then
                gt=(em*h*syc**2+em*h*syc**2*num-em*h*syc**2*num**2+em*&
                h* syt**2*num**2-em*h*syt**2-em*h*syt**2*num-2.d0*&
                pendc*syc**2-4.d0* pendc*syc**2*num+2.d0*pendc*syc**2*&
                num**2+4.d0*pendc*syc**2*num**3+2.d0*pendc*syt**2*&
                num**2+2.d0*pendc*syt**2*num**3)/(em*h* (-syc**2-syc**&
                2*num+3.d0*syc**2*num**2+3.d0*syt**2*num**2-syt**2-&
                syt**2*num))
                gc=1.d0-(1.d0-gt)*(syt**2*(1.d0-num)*(1.d0+2.d0*num)-&
                syc**2*num**2)/(syc**2*(1.d0-num)*(1.d0+2.d0*num)-&
                syt**2*num**2)
            else if (icompr .eq. 2) then
                gt=-(em*h*gc-2.d0*pendc-2.d0*pendc*num)/(em*h)
                syc=sqrt(-(num-num**2-gt*num**2-gc-gc*num+2.d0*gc*&
                num**2+1.d0)*(gc*num**2-num+gt+num**2-1.d0+gt*num-&
                2.d0*gt*num**2))*syt/(num-num**2-gt*num**2-gc-gc*num+&
                2.d0*gc*num**2+1.d0)
            endif
! - METHODE 3
        else
            gt=pendc/((em*h)/2.d0/(1.d0+num))
            if (icompr .eq. 1) then
                gc=1.d0-(1.d0-gt)*(syt**2*(1.d0-num)*(1.d0+2.d0*num)-&
                syc**2*num**2)/(syc**2*(1.d0-num)*(1.d0+2.d0*num)-&
                syt**2*num**2)
            else if (icompr .eq. 2) then
                syc=sqrt(-(num-num**2-gt*num**2-gc-gc*num+2.d0*&
                gc*num**2+1.d0)*(num**2-1.d0-num+gt+gt*num-2.d0*gt*&
                num**2+gc* num**2))*syt/(num-num**2-gt*num**2-gc-gc*&
                num+2.d0*gc*num**2 +1.d0)
            endif
        endif
    endif
!
!
! - CALCUL DES PARAMETRES D'ENDOMMAGEMENT EN FLEXION
    if (iendo .eq. 1) then
        xf=12.d0*pendf/ef/h**3
        gf=(xf*(1.d0+nuf)*(1.d0+nuf-2.d0*nuf**2+nuf**3)-nuf**3)/(1.d0+&
        2.d0*nuf-2.d0*nuf**3-xf*nuf**2*(1.d0-nuf**2))
    else if (iendo .eq. 2) then
        gf1=1.d0/2.d0*(12.d0*pendf*nuf**2+12.d0*pendf*nuf-ef*h**3+&
        sqrt (144.d0*pendf**2*nuf**4+288.d0*pendf**2*nuf**3-24.d0*ef*&
        h**3*pendf*nuf**2+144.d0*pendf**2*nuf**2+24.d0*pendf*nuf*ef*&
        h**3+ef**2* h**6-48.d0*ef*h**3*nuf**3*pendf))/(ef*h**3*nuf)
        gf2=1.d0/2.d0*(12.d0*pendf*nuf**2+12.d0*pendf*nuf-ef*h**3-&
        sqrt(144.d0*pendf**2*nuf**4+288.d0*pendf**2*nuf**3-24.d0*ef*&
        h**3* pendf*nuf**2+144.d0*pendf**2*nuf**2+24.d0*pendf*nuf*ef*&
        h**3+ ef**2*h**6-48.d0*ef*h**3*nuf**3*pendf))/(ef*h**3*nuf)
        gf=max(gf1,gf2)
    else
        gf=pendf/pelasf
    endif
!
    if (gt .lt. 0.d0) then
        call u2mesk('A', 'ALGORITH6_4', 1, 'GAMMAT')
    endif
    if (gc .lt. 0.d0) then
        call u2mesk('A', 'ALGORITH6_4', 1, 'GAMMAC')
    endif
    if (gf .lt. 0.d0) then
        call u2mesk('A', 'ALGORITH6_4', 1, 'GAMMAF')
    endif
!
end subroutine
