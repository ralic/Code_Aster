subroutine resgra(mat, matf, vcine, niter, epsi,&
                  criter, nsecm, rsolu, solveu, istop,&
                  iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/csmbgg.h"
#include "asterfort/gcpc.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrconl.h"
#include "asterfort/mtdscr.h"
#include "asterfort/pcmump.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: mat, matf, vcine
    integer :: niter, nsecm
    real(kind=8) :: epsi, rsolu(*)
    character(len=19) :: criter, solveu
    integer :: istop, iret
!----------------------------------------------------------------------
!     ROUTINE DE HAUT NIVEAU DE RESOLUTION PAR UNE METHODE DE GRADIENT
!     CONJUGUE (GCPC)
!----------------------------------------------------------------------
! IN/JXIN  K19 MAT    : MATR_ASSE PREMIER MEMBRE DU SYSTEME LINEAIRE
! IN/JXIN  K19 MATF   : MATR_ASSE DE PRECONDITIONNEMENT
! IN/JXIN  K*  VCINE  : CHAMP ASSOCIE AUX CHARGES CINEMATIQUES (OU ' ')
! IN       I   NITER  : NOMBRE MAXIMUM D'ITERATIONS
! IN       R   EPSI   : PARAMETRE D'ERREUR
! IN/JXOUT K19 CRITER : SD_CRITER (CRITERES DE CONVERGENCE)
! IN       I   NSECM  : NOMBRE DE SECONDS MEMBRES
! IN/OUT   R   RSOLU(*,NSECM)  :
!        EN ENTREE : VECTEUR DE REELS CONTENANT LES SECONDS MEMBRES
!        EN SORTIE : VECTEUR DE REELS CONTENANT LES SOLUTIONS
! IN       K19 SOLVEU : SD_SOLVEUR
! IN       I   ISTOP  : COMPORTEMENT EN CAS D'ERREUR
! OUT      I   IRET   : CODE RETOUR
!----------------------------------------------------------------------
!----------------------------------------------------------------------
    complex(kind=8) :: cbid
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    character(len=19) :: kstoc, kstocf
    character(len=19) :: vcin19, matas, matfac
    character(len=4) :: type
    character(len=24) :: precon
    integer :: ifm, niv, ier,   idip,  neq, nblc
    integer :: idac, idinpc, idippc, idacpc
    integer ::   k, lmat, kdeb, ieq, istop_solv
    integer, dimension(:), pointer :: slvi => null()
    real(kind=8), pointer :: w1(:) => null()
    real(kind=8), pointer :: w2(:) => null()
    real(kind=8), pointer :: w3(:) => null()
    real(kind=8), pointer :: w4(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: smde(:) => null()
    character(len=24), pointer :: refaf(:) => null()
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: in(:) => null()
    integer, pointer :: perm(:) => null()
!
!----------------------------------------------------------------------
!     DEBUT
    call jemarq()
!
    call infniv(ifm, niv)
!
!     0- INITIALISATIONS :
!     -----------------------------------
    matas=mat
    matfac=matf
!
    call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
    precon=slvk(2)
!
!
!     1- MATRICE :
!     -----------------------------------
    call mtdscr(matas)
    call jeveuo(matas//'.&INT', 'L', lmat)
    neq=zi(lmat+2)
!
!
!     3- SI CHARGE CINEMATIQUE :
!     -----------------------------
    if (vcine .ne. ' ') then
        vcin19=vcine
        call jeexin(vcin19//'.VALE', ier)
        if (ier .eq. 0) then
            call utmess('F', 'ALGELINE3_34', sk=vcin19)
        endif
        call jeveuo(vcin19//'.VALE', 'L', vr=vale)
        do 10,k=1,nsecm
            kdeb=(k-1)*neq+1
            cbid = dcmplx(0.d0, 0.d0)
            call csmbgg(lmat, rsolu(kdeb), vale, [cbid], [cbid], 'R')
10      continue
    endif
!
!
!     4- MISE A L'ECHELLE DES "LAGR" DANS LE SECOND MEMBRE :
!     ------------------------------------------------------
    call mrconl('MULT', lmat, 0, 'R', rsolu,&
                nsecm)
!
!
!     5- RECUPERATION DE LA MATRICE ASSEMBLEE :
!     ------------------------------------------------
    call jeveuo(matas//'.REFA', 'E', vk24=refa)
    kstoc=refa(2)(1:14)//'.SMOS'
    call jeexin(kstoc//'.SMDI', ier)
    if (ier .eq. 0) then
        call utmess('F', 'ALGELINE3_21', sk=matas)
    endif
    call jeveuo(kstoc//'.SMDI', 'L', vi=in)
    call jeveuo(kstoc//'.SMHC', 'L', idip)
    call jeveuo(kstoc//'.SMDE', 'L', vi=smde)
    neq=smde(1)
    if (niter .eq. 0) niter=max(10,neq/2)
    nblc=smde(3)
    if (nblc .ne. 1) then
        call utmess('F', 'ALGELINE3_22')
    endif
    call jelira(jexnum(matas//'.VALM', 1), 'TYPE', cval=type)
    if (type .ne. 'R') then
        call utmess('F', 'ALGELINE3_37')
    endif
!
    call jeveuo(jexnum(matas//'.VALM', 1), 'L', idac)
!
!
!     6- RECUPERATION DE LA MATRICE DE PRECONDITIONNEMENT:
!     -----------------------------------------------------
    if (precon(1:8) .eq. 'LDLT_INC') then
        call jeexin(matfac//'.REFA', ier)
        if (ier .eq. 0) then
            call utmess('F', 'ALGELINE3_38')
        endif
!
        call jeveuo(matfac//'.REFA', 'L', vk24=refaf)
        kstocf=refaf(2)(1:14)//'.SMOS'
        call jeveuo(kstocf//'.SMDI', 'L', idinpc)
        call jeveuo(kstocf//'.SMHC', 'L', idippc)
        call jeveuo(jexnum(matfac//'.VALM', 1), 'L', idacpc)
        call jeveuo(matfac//'.PERM','L',vi=perm)
    else
        idinpc=1
        idippc=1
        idacpc=1
    endif
!
!
!     7- CREATION DE 3 VECTEURS DE TRAVAIL
!     ------------------------------------------------
    AS_ALLOCATE(vr=w1, size=neq)
    AS_ALLOCATE(vr=w2, size=neq)
    AS_ALLOCATE(vr=w3, size=neq)
!
!
!
!     9- RESOLUTION EFFECTIVE ---
!     ---------------------------------
    do k=1,nsecm
    AS_ALLOCATE(vr=w4, size=neq)
!
    kdeb=(k-1)*neq+1
    istop_solv = istop
!
    if ( precon(1:7) == 'LDLT_SP' ) then
!   ACTIVATION DE LA SECONDE CHANCE : stop_singulier = non
        istop_solv = 2
    endif
 !
    call gcpc(neq, in, zi4(idip), zr(idac), zi(idinpc), perm,&
              zi4(idippc), zr(idacpc), rsolu(kdeb), w4, w1,&
              w2, w3, 0, niter, epsi,&
              criter, solveu, matas, istop_solv,&
              iret)
!
!     9-1 SECONDE CHANCE AVEC LE PRECONDITIONNEUR LDLT_SP
!     (cf ap2foi)
    if (( iret == 1 ).and.(precon(1:7) == 'LDLT_SP' )) then
!       ON ACTUALISE LE PRECONDITIONNEUR
        call utmess('I', 'ALGELINE4_63')
!   -- bascule pour la mesure du temps CPU : RESOUD -> PRERES :
        call uttcpu('CPU.RESO.5', 'FIN', ' ')
        call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
!       slvi(5) = nombre d'itérations pour atteindre la convergence du solveur linéaire.
!       si :
!       - slvi(5) = 0 (on résout pour la première fois),
!       - slvi(5) > reac_precond (la résolution linéaire précédente a demandé
!                                 "trop" d'itérations),
!       alors il faut effectuer le calcul du préconditionneur LDLT_SP (voir pcmump)
        call jeveuo(solveu//'.SLVI', 'E', vi=slvi)
        slvi(5) = 0
!       reset matrix state
        refa(8) = ' '
        call pcmump(matas, solveu, iret)
        if (iret .ne. 0) then
             call utmess('F', 'ALGELINE5_76')
        endif
    !
    !
    !   -- bascule pour la mesure du temps CPU : PRERES -> RESOUD :
    call uttcpu('CPU.RESO.4', 'FIN', ' ')
    call uttcpu('CPU.RESO.5', 'DEBUT', ' ')
!
!       PUIS ON RESOUT A NOUVEAU
        call gcpc(neq, in, zi4(idip), zr(idac), zi(idinpc), perm,&
              zi4(idippc), zr(idacpc), rsolu(kdeb), w4, w1,&
              w2, w3, 0, niter, epsi,&
              criter, solveu, matas, istop, &
              iret)

    endif
!
    do ieq=1,neq
    rsolu(kdeb-1+ieq)=w4(ieq)
    enddo
    AS_DEALLOCATE(vr=w4)
    end do
!
!
!     10- MISE A L'ECHELLE DES LAGRANGES DANS LA SOLUTION :
!     -----------------------------------------------------
    call mrconl('MULT', lmat, 0, 'R', rsolu,&
                nsecm)
!
!
    AS_DEALLOCATE(vr=w1)
    AS_DEALLOCATE(vr=w2)
    AS_DEALLOCATE(vr=w3)
!
    call jedema()
end subroutine
