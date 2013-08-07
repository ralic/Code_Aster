subroutine pcldlt(matf, mat, niremp, bas)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtmchc.h"
#include "asterfort/pccoef.h"
#include "asterfort/pcfact.h"
#include "asterfort/pcstru.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: matf, mat, bas
!-----------------------------------------------------------------------
!  FONCTION  :
!     CREATION D'UNE MATRICE DE PRECONDITIONNEMENT MATF
!     PAR FACTORISATION LDLT PLUS OU MOINS COMPLETE DE LA MATRICE MAT
!     STOCKEE SOUS FORME MORSE.
!     ON PEUT CHOISIR LE DEGRE DE REMPLISSAGE : NIREMP
!
!-----------------------------------------------------------------------
! OUT K*  MATF   : NOM DE LA MATR_ASSE DE PRECONDITIONNEMENT
!                  REMARQUE : CE N'EST PAS VRAIMENT UNE MATR_ASSE :
!                             ELLE A UN STOCKAGE MORSE "ETENDU"
!                             ET ELLE CONTIENT UNE FACTORISEE LDLT !
! IN  K*  MAT    : NOM DE LA MATR_ASSE A PRECONDITIONNER
! IN  I   NIREMP : NIVEAU DE REMPLISSAGE VOULU POUR MATF
! IN  K*  BAS    : NOM DE LA BASE SUR LAQUELLE ON CREE MATF 'G' OU 'V'
!-----------------------------------------------------------------------
!     FONCTIONS JEVEUX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    logical :: complt
    character(len=1) :: base
    integer :: iret, jsmdi, jsmhc, jsmde, nequ, ncoef, nblc
    integer :: jvalm, idv, i, nzmax, jicpd, jicplx, niremp
    integer :: jicpcx, jsmdi1, jsmhc1, ier, k, jsmdif, jsmhcf, jvalf, jvect
    integer :: jrefa, jrefaf
    real(kind=8) :: dnorm, epsi
    character(len=19) :: matfac, matas
    character(len=1) :: tysca
    character(len=14) :: nu, nuf
    character(len=24) :: noobj
    character(len=8) :: ma
!----------------------------------------------------------------------
!     DEBUT DES INSTRUCTIONS
    call jemarq()
!
    matas = mat
    matfac = matf
    base = bas
!
!
!     -- DDLS ELIMINES :
    call jeveuo(matas//'.REFA', 'L', jrefa)
    ASSERT(zk24(jrefa-1+3).ne.'ELIMF')
    if (zk24(jrefa-1+3) .eq. 'ELIML') call mtmchc(matas, 'ELIMF')
    ASSERT(zk24(jrefa-1+3).ne.'ELIML')
!
!
!
!     1. CALCUL DE : MA,NU,JSMDI,JSMHC,JSMDE
!         NEQU,NCOEF
!         + QQUES VERIFS
!     ------------------------------------------
    call jeveuo(matas//'.REFA', 'L', jrefa)
    ma = zk24(jrefa-1+1)(1:8)
    nu = zk24(jrefa-1+2)(1:14)
!
    call jeexin(nu//'.SMOS.SMDI', iret)
    if (iret .eq. 0) call u2mesk('F', 'ALGELINE3_21', 1, matas)
!
    call jeveuo(nu//'.SMOS.SMDI', 'L', jsmdi)
    call jeveuo(nu//'.SMOS.SMHC', 'L', jsmhc)
    call jeveuo(nu//'.SMOS.SMDE', 'L', jsmde)
    nequ = zi(jsmde-1+1)
    ncoef = zi(jsmde-1+2)
!
    nblc = zi(jsmde-1+3)
    if (nblc .ne. 1) call u2mess('F', 'ALGELINE3_22')
!
    call jelira(jexnum(matas//'.VALM', 1), 'TYPE',cval=tysca)
    if (tysca .eq. 'C') call u2mess('F', 'ALGELINE3_23')
!
!
!
!     1. CREATION DU NUME_DDL ASSOCIE A MATFAC :
!     ------------------------------------------
!
!     DETERMINATION DU NOM DE LA SD CACHEE NUME_DDL
    noobj ='12345678.NU000.NUME.PRNO'
    call gnomsd(' ', noobj, 12, 14)
    nuf=noobj(1:14)
    call copisd('NUME_DDL', base, nu, nuf)
!
!
!     2. CREATION DE MATFAC.REFA
!     ---------------------------
    call jedetr(matfac//'.REFA')
    call wkvect(matfac//'.REFA', base//' V K24 ', 11, jrefaf)
    zk24(jrefaf-1+11)='MPI_COMPLET'
    zk24(jrefaf-1+1) = ma
    zk24(jrefaf-1+2) = nuf
    zk24(jrefaf-1+9) = 'MS'
    zk24(jrefaf-1+10) = 'NOEU'
!
!
!     2. CALCUL DE EPSI POUR PCFACT ET ALLOCATION DE .VTRAVAIL:
!     ---------------------------------------------------------
    call jeveuo(jexnum(matas//'.VALM', 1), 'L', jvalm)
    call wkvect('&&PCLDLT.VTRAVAIL', 'V V R', nequ, idv)
    dnorm = 0.d0
    do 10 i = 1, nequ
        dnorm = max(abs(zr(jvalm-1+zi(jsmdi-1+i))),dnorm)
10  end do
    epsi = 1.d-16*dnorm
    call jelibe(jexnum(matas//'.VALM', 1))
!
!
!     3. ON BOUCLE SUR PCSTRU JUSQU'A TROUVER LA TAILLE DE LA
!        FUTURE FACTORISEE :
!     ------------------------------------------------
    nzmax = ncoef
    call wkvect('&&PCLDLT.SMDIF', 'V V I', nequ+1, jsmdi1)
!
    do 7778,k=1,2*niremp+2
    call wkvect('&&PCLDLT.ICPD', 'V V I', nequ, jicpd)
    call wkvect('&&PCLDLT.ICPLX', 'V V I', nequ+1, jicplx)
    call jedetr('&&PCLDLT.SMHCF')
    call wkvect('&&PCLDLT.SMHCF', 'V V S', 2*nzmax, jsmhc1)
    call wkvect('&&PCLDLT.ICPCX', 'V V I', nzmax, jicpcx)
!
    call pcstru(nequ, zi(jsmdi), zi4(jsmhc), zi(jsmdi1), zi4(jsmhc1),&
                zi(jicpd), zi(jicpcx), zi(jicplx), niremp, complt,&
                nzmax, 0, ier)
!
    call jedetr('&&PCLDLT.ICPLX')
    call jedetr('&&PCLDLT.ICPCX')
    call jedetr('&&PCLDLT.ICPD')
    if (ier .eq. 0) goto 7779
    nzmax=ier
    7778 end do
    call u2mess('F', 'ALGELINE3_24')
7779  continue
!
!
!     -- ON MET A JOUR NUF.SMDI ET NUF.SMHC  :
!     ------------------------------------------------
    call jeveuo(nuf//'.SMOS.SMDI', 'E', jsmdif)
    do 20,k = 1,nequ
    zi(jsmdif-1+k) = zi(jsmdi1-1+k)
    20 end do
    call jedetr('&&PCLDLT.SMDIF')
!
    call jedetr(nuf//'.SMOS.SMHC')
    call wkvect(nuf//'.SMOS.SMHC', base//' V S', nzmax, jsmhcf)
    do 30,k = 1,nzmax
    zi4(jsmhcf-1+k) = zi4(jsmhc1-1+k)
    30 end do
    call jedetr('&&PCLDLT.SMHCF')
!
!
!
!     -- ON ALLOUE MATFAC.VALM :
!     ------------------------------------------------
    call jedetr(matfac//'.VALM')
    call jecrec(matfac//'.VALM', base//' V '//tysca, 'NU', 'DISPERSE', 'CONSTANT',&
                1)
    call jeecra(matfac//'.VALM', 'LONMAX', nzmax)
    call jecroc(jexnum(matfac//'.VALM', 1))
!
!
!     -- ON INJECTE MATAS.VALM DANS MATFAC.VALM :
!     ------------------------------------------------
    call jeveuo(jexnum(matas//'.VALM', 1), 'L', jvalm)
    call jeveuo(jexnum(matfac//'.VALM', 1), 'E', jvalf)
    call pccoef(nequ, zi(jsmdi), zi4(jsmhc), zr(jvalm), zi(jsmdif),&
                zi4(jsmhcf), zr(jvalf), zr(idv))
    call jelibe(jexnum(matas//'.VALM', 1))
!
!
!     -- ON FACTORISE MATFAC.VALM :
!     ------------------------------------------------
    call wkvect('&&PCLDLT.VECT', 'V V R', nequ, jvect)
    call pcfact(matas, nequ, zi(jsmdif), zi4(jsmhcf), zr(jvalf),&
                zr(jvalf), zr(jvect), epsi)
    call jedetr('&&PCLDLT.VECT')
!
    call jedetr('&&PCLDLT.VTRAVAIL')
    call jedema()
end subroutine
