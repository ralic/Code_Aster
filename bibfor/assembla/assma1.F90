subroutine assma1(matas, ldist)
    implicit none
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
!--------------------------------------------------------------
! BUT : METTRE A L'ECHELLE LES LIGNES ET COLONNES D'UNE MATR_ASSE
!       CORRESPONDANT AUX DDLS DE LAGRANGE
!
! IN/JXVAR : MATAS (K19) : SD_MATR_ASSE  :
!    -- CREATION DE L'OBJET .CONL
!    -- MODIFICATION DE L'OBJET .VALM
! IN LDIST (LOGICAL): INDIQUE SI LE CALCUL EST DISTRIBUE AU SENS
!                     DONNEE INCOMPLETE PAR PROC
!---------------------------------------------------------------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/echmat.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: matas
!---------------------------------------------------------------
    logical :: lmnsy, exilag, ldist
    integer :: jsmdi, nsmhc, jdelgg, jdelgl, jsmhc, ng, nz, n, imatd
    integer :: ilig, jcol, kterm, nlong, jrefa, nvale, jvalm1, jvalm2, jconl
    character(len=1) ::  ktyp, base1
    character(len=14) :: nonu
    character(len=19) :: mat19
    real(kind=8) :: rmin, rmax, rcoef
!=================================================================
    call jemarq()
!
!
!
! 1. *  MISE EN MEMOIRE DES OBJETS JEVEUX
!    *  CALCUL DE  :
!        N  : NOMBRE D'EQUATIONS
!        NZ : NOMBRE DE TERMES NON NULS DANS LA MOITIE SUPERIEURE
!        LMNSY : .TRUE.  : LA MATRICE EST NON SYMETRIQUE
!                .FALSE. : LA MATRICE EST SYMETRIQUE
!        KTYP  : 'R'/'C'
!        BASE1 : 'G'/'V'
!    *  QUELQUES VERIFICATIONS DE COHERENCE
! ---------------------------------------------------------------
    mat19=matas
    call jeveuo(mat19//'.REFA', 'L', jrefa)
    nonu=zk24(jrefa-1+2)(1:14)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', n)
    call jeveuo(nonu//'.SMOS.SMDI', 'L', jsmdi)
    nz=zi(jsmdi-1+n)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
    ASSERT(nz.le.nsmhc)
!
    call jeveuo(nonu//'.NUME.DELG', 'L', jdelgg)
    call jelira(nonu//'.NUME.DELG', 'LONMAX', ng)
    call jeexin(nonu//'.NUML.DELG', imatd)
    if (imatd .ne. 0) then
        call jeveuo(nonu//'.NUML.DELG', 'L', jdelgl)
    else
        jdelgl=jdelgg
        ASSERT(ng.eq.n)
    endif
!
    call jelira(mat19//'.VALM', 'TYPE', cval=ktyp)
    call jelira(mat19//'.VALM', 'CLAS', cval=base1)
    call jeveuo(jexnum(mat19//'.VALM', 1), 'E', jvalm1)
    call jelira(jexnum(mat19//'.VALM', 1), 'LONMAX', nlong)
    ASSERT(nlong.eq.nz)
!
    lmnsy=.false.
    call jelira(mat19//'.VALM', 'NMAXOC', nvale)
    if (nvale .eq. 2) lmnsy=.true.
!
    if (lmnsy) then
        call jeveuo(jexnum(mat19//'.VALM', 2), 'E', jvalm2)
        call jelira(jexnum(mat19//'.VALM', 2), 'LONMAX', nlong)
        ASSERT(nlong.eq.nz)
    endif
!
!
!     CALCUL DE EXILAG : .TRUE. : IL EXISTE DES DDLS DE LAGRANGE
    exilag=.false.
    do 10,jcol=1,n
    if (zi(jdelgl-1+jcol) .lt. 0) then
        exilag=.true.
        goto 10
    endif
    10 end do
    if (imatd .ne. 0) then
        exilag=.true.
    endif
!
!     -- S'IL N'Y A PAS DE LAGRANGE, IL N'Y A RIEN A FAIRE :
    if (.not.exilag) goto 40
!
! 2.  CALCUL DU COEFFICIENT DE CONDITIONNEMENT DES LAGRANGES (RCOEF)
! -------------------------------------------------------------------
    call echmat(mat19, ldist, rmin, rmax)
!     -- PARFOIS, LA MATRICE EST == 0.
    if (rmax .eq. 0.d0) then
        rcoef=1.d0
    else
        rcoef=0.5d0*(rmin+rmax)
    endif
!
! ---------------------------------------------------------------
    call wkvect(mat19//'.CONL', base1//' V R', ng, jconl)
    do 20,jcol=1,ng
    if (zi(jdelgg-1+jcol) .eq. 0) then
        zr(jconl-1+jcol)=1.d0
    else
        zr(jconl-1+jcol)=rcoef
    endif
    20 end do
!
!
! 4.  MISE A L'ECHELLE DE LA MATRICE
! ---------------------------------------------------------------
    jcol=1
    do 30,kterm=1,nz
    if (zi(jsmdi-1+jcol) .lt. kterm) jcol=jcol+1
    ilig=zi4(jsmhc-1+kterm)
    if (zi(jdelgl-1+jcol)+zi(jdelgl-1+ilig) .lt. 0) then
        if (ktyp .eq. 'R') then
            zr(jvalm1-1+kterm)=rcoef*zr(jvalm1-1+kterm)
        else
            zc(jvalm1-1+kterm)=rcoef*zc(jvalm1-1+kterm)
        endif
        if (lmnsy) then
            if (ktyp .eq. 'R') then
                zr(jvalm2-1+kterm)=rcoef*zr(jvalm2-1+kterm)
            else
                zc(jvalm2-1+kterm)=rcoef*zc(jvalm2-1+kterm)
            endif
        endif
    endif
    30 end do
    ASSERT(jcol.eq.n)
!
!
40  continue
    call jedema()
end subroutine
