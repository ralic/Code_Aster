subroutine nmcpl2(compor, typmod, option, optio2, cp,&
                  nvv, crit, deps, dsidep, ndim,&
                  sigp, vip, iret)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     CONTRAINTES PLANES PAR LA METHODE DE BORST / CONDENSATION STATIQUE
!     POUR LES COMPORTEMENTS QUI N'INTEGRENT PAS LES CONTRAINTES PLANES
!     ATTENTION : POUR BIEN CONVERGER, IL FAUT REACTUALISER LA MATRICE
!     TANGENTE. DE PLUS, IL FAUT AJOUTER 4 VARIABLES INTERNES
!
! IN  TYPMOD  : TYPE DE MODELISATION
!     OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
!     OPTIO2  : OPTION MODIFIEE POUR TOUJOURS CALCULER K TANGENT
!     CP      : LOGIQUE INDIQUANT SI C_PLAN METHODE DE BORST
!     NVV     : NOMBRE DE VRAIES VARIABLES INTERNES
!     CRIT    : CRITERES DE CONVERGENCE LOCAUX
!                               (3) = VALEUR TOLERANCE DE CONVERGENCE
!                                     (RESI_INTE_RELA == RESCREL)
!     DEPS    : INCREMENT DE DEFORMATION TOTALE :
!               DEPS(T) = DEPS(MECANIQUE(T)) + DEPS(DILATATION(T))
! VAR DSIDEP  : MATRICE TANGENTE CARREE
! IN  NDIM    : DIMENSION DE L'ESPACE
!               3 : 3D , 2 : D_PLAN ,AXIS OU  C_PLAN
! VAR SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! VAR VIP     : LES 4 DERNIERES SONT RELATIVES A LA METHODE DE BORST
!
    implicit none
#include "asterf_types.h"
    integer :: ndimsi, k, l, iret, ndim, nvv, nbvari, cp
    character(len=8) :: typmod(*)
    character(len=16) :: option, optio2
    character(len=16) :: compor(*)
    real(kind=8) :: vip(*), depzz, deps(*), rac2, crit(*), dsidep(6, *), sigp(*)
    real(kind=8) :: depy, depz, d11, d22, d33, d12, d13, d21, d23, d31, d32
    real(kind=8) :: delta, dy, dz
    real(kind=8) :: sigy, sigz, depx, sigx, vip1, vip2, vip3, vip4, signul
    aster_logical :: vecteu
!
    real(kind=8) :: d21eps, scm(4), sigpeq, precr, prec
!
    rac2 = sqrt(2.d0)
    ndimsi = 2*ndim
    iret=0
    signul=crit(3)
    vecteu = option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA'
!
!   calcul de la precision
    prec=crit(8)
    precr=abs(prec)
    if (vecteu) then
        if (prec .gt. 0.d0) then
!           PRECISION RELATIVE
            sigpeq=0.d0
            do k = 1, ndimsi
               sigpeq = sigpeq + sigp(k)**2
            enddo
            sigpeq = sqrt(sigpeq)
            if (sigpeq .lt. signul) then
                precr=prec
            else
                precr=prec*sigpeq
            endif
        else
    !       PRECISION ABSOLUE
            precr=abs(prec)
        endif
     endif      

 !
    if (cp .eq. 2) then
!
!       ON REMET LES CHOSES DANS L'ETAT OU ON LES A TROUVEES
        nbvari=nvv+4
        write (compor(2),'(I16)') nbvari
        typmod(1)='C_PLAN'
        option=optio2
!
        if ((vecteu).and.(abs(dsidep(3,3)).gt.precr)) then

            depzz=deps(3)
            d22=dsidep(3,3)
            d21eps=dsidep(3,1)*deps(1)+dsidep(3,2)*deps(2) +dsidep(3,&
            4)*deps(4)/rac2
            vip(nvv+1)=depzz+d21eps/d22-sigp(3)/d22
            vip(nvv+2)=dsidep(3,1)/d22
            vip(nvv+3)=dsidep(3,2)/d22
            vip(nvv+4)=dsidep(3,4)/d22
!
            scm(1)=-dsidep(1,3)*sigp(3)/d22
            scm(2)=-dsidep(2,3)*sigp(3)/d22
            scm(3)=0.d0
            scm(4)=-dsidep(4,3)*sigp(3)/d22*rac2
!
            do 130 k = 1, ndimsi
                sigp(k)=sigp(k)+scm(k)
130         continue
!
!
            if (abs(sigp(3)) .gt. precr) then
                iret=3
            endif
!
        endif
!
!         IF (MATRIC) THEN
        if (option .eq. 'FULL_MECA') then
            do 136 k = 1, ndimsi
!
                if (k .eq. 3) goto 136
!
                do 137 l = 1, ndimsi
!
                    if (l .eq. 3) goto 137
                    if (abs(dsidep(3,3)).gt.precr) then
                        dsidep(k,l)=dsidep(k,l) - 1.d0/dsidep(3,3)*dsidep(k,3)*dsidep(3,l)
                    endif
!
137             continue
136         continue
!
        endif
!
    else if (cp.eq.1) then
!
!        ON REMET LES CHOSES DANS L'ETAT OU ON LES A TROUVEES
        nbvari=nvv+4
        write (compor(2),'(I16)') nbvari
        typmod(1)='COMP1D'
        option=optio2
        iret=0
!
        depx=deps(1)
        depy=deps(2)
        depz=deps(3)
        d11=dsidep(1,1)
        d12=dsidep(1,2)
        d13=dsidep(1,3)
        d21=dsidep(2,1)
        d22=dsidep(2,2)
        d23=dsidep(2,3)
        d31=dsidep(3,1)
        d32=dsidep(3,2)
        d33=dsidep(3,3)

        if ((vecteu).and.(abs(d22*d33-d32*d23).gt.precr)) then
!
            delta=d22*d33-d32*d23
            dy=d23*d31-d21*d33
            dz=d32*d21-d31*d22
            sigx=sigp(1)
            sigy=sigp(2)
            sigz=sigp(3)
            vip1=depy+(d23*sigz-d33*sigy-dy*depx)/delta
            vip2=dy/delta
            vip3=depz+(d32*sigy-d22*sigz-dz*depx)/delta
            vip4=dz/delta
!
            vip(nvv+1)=vip1
            vip(nvv+2)=vip2
            vip(nvv+3)=vip3
            vip(nvv+4)=vip4
!
            scm(1)=(d12*d23-d22*d13)*sigz+(d13*d32-d12*d33)*sigy
            scm(1)=scm(1)/delta
            scm(2)=0.d0
            scm(3)=0.d0
            scm(4)=0.d0
!
            do 140 k = 1, ndimsi
                sigp(k)=sigp(k)+scm(k)
140         continue
!
            if (abs(sigp(2)) .gt. precr) iret=3
            if (abs(sigp(3)) .gt. precr) iret=3
!
        endif
!
        if ((option .eq. 'FULL_MECA').and.(abs(delta).gt.precr)) then
            dsidep(1,1)=d11+(d12*dy+d13*dz)/delta
        endif
!
    endif
!
end subroutine
