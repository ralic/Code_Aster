subroutine fointc(codmes, nomf, nbpu, nompu, valpu,&
                  resure, resuim, ier)
    implicit  none
#include "jeveux.h"
!
#include "asterc/fiintfc.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/folocx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: nbpu, ier
    real(kind=8) :: valpu(*), resure, resuim
    character(len=*) :: codmes, nomf, nompu(*)
!     ------------------------------------------------------------------
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
!     INTERPOLATION POUR FONCTION COMPLEXE A VARIABLE REELLE
!
!     CETTE ROUTINE EST DUPLIQUEE,AVEC QUELQUES LIGNES EN MOINS
!     DANS FITABU. VEILLER A GARDER LA CONCORDANCE EN CAS DE
!     MODIFICATION.
!     ------------------------------------------------------------------
! IN  NOMF   : NOM DE LA FONCTION OU DE LA NAPPE
! IN  NBPU   : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
! IN  NOMPU  : NOMS DES PARAMETRES "UTILISATEUR"
! IN  VALPU  : VALEURS DES PARAMETRES "UTILISATEUR"
! OUT RESURE : RESULTAT DE L'INTERPOLATION (PARTIE REELLE)
! OUT RESUIM : RESULTAT DE L'INTERPOLATION (PARTIE IMAGINAIRE)
! OUT IER    : CODE RETOUR
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: lprol, i, i1, i2, lvale, lfonc, nbvale
    real(kind=8) :: linlin, linlog, loglog, loglin, x, x1, y1, x2, y2
    real(kind=8) :: epsi, tole, valr, resu(2)
    character(len=1) :: coli, k1bid
    character(len=24) :: interp, prolgd
    character(len=19) :: nomfon
    character(len=24) :: chprol, chvale
!     ------------------------------------------------------------------
    integer :: mxsave
    parameter   (mxsave=4)
    integer :: isvnxt, nextsv(mxsave), isvind, isave
    character(len=24) :: svprgd(mxsave)
    character(len=24) :: svinte(mxsave)
    character(len=19) :: svnomf(mxsave)
    save         svnomf,svprgd,svinte
    save         isvnxt,isvind
    data         svnomf/mxsave*'????????'/
    data         isvnxt/mxsave/
    data         nextsv/2,3,4,1/,isvind/1/
!     ------------------------------------------------------------------
!     FONCTION EN LIGNE
!
    linlin(x,x1,y1,x2,y2)= y1+(x-x1)*(y2-y1)/(x2-x1)
    linlog(x,x1,y1,x2,y2)=exp(log(y1)+(x-x1)*(log(y2)-log(y1))&
     &                                        /(x2-x1))
    loglog(x,x1,y1,x2,y2)=exp(log(y1)+(log(x)-log(x1))*(log(y2)&
     &                                     -log(y1))/(log(x2)-log(x1)))
    loglin(x,x1,y1,x2,y2)=y1+(log(x)-log(x1))*(y2-y1)&
     &                                         /(log(x2)-log(x1))
!     ------------------------------------------------------------------
    call jemarq()
!
    ier = 0
    epsi = sqrt ( r8prem() )
    resure = r8vide()
    resuim = r8vide()
    nomfon = nomf
    chprol = nomfon//'.PROL'
    chvale = nomfon//'.VALE'
    call jeveuo(chprol, 'L', lprol)
!
! --- CALCUL DE LA FONCTION INTERPRETEE ---
!
    if (zk24(lprol) .eq. 'INTERPRE') then
!        -- SI LA FONCTION EST REELLE, FIINTF NE CALCULE QUE
!           RESU(1). IL FAUT DONC INITIALISER RESU(2).
        resu(2)=0.d0
        call fiintfc(nomf, nbpu, nompu, valpu, ier,&
                     'A', resu)
        if (ier .gt. 0) then
            ier = 110
            goto 999
        endif
        resure=resu(1)
        resuim=resu(2)
        goto 999
!
    endif
!
! --- LES AUTRES TYPES DE FONCTION ---
!
    valr = valpu(1)
!
    do i = 1, mxsave
        if (nomfon .eq. svnomf(i)) then
            isave = i
            goto 11
        endif
    end do
!
! --- MEMORISATION DES INFORMATIONS NOUVELLES
!
    isvnxt = nextsv(isvnxt)
    isave = isvnxt
    call jeveuo(chprol, 'L', lprol)
    svinte(isave) = zk24(lprol+1)
    svprgd(isave) = zk24(lprol+4)
!
11  continue
    interp = svinte(isave)
    prolgd = svprgd(isave)
!
    call jeveuo(chvale, 'L', lvale)
    call jelira(chvale, 'LONUTI', nbvale, k1bid)
    nbvale = nbvale / 3
    lfonc = lvale + nbvale - 1
!
    i = isvind
    call folocx(zr(lvale), nbvale, valr, prolgd, i,&
                epsi, coli, ier)
    if (ier .ne. 0) goto 999
!
    if (coli .eq. 'C') then
        i1 = 1 + 2 * ( i - 1 )
        resure = zr(lfonc+i1)
        resuim = zr(lfonc+i1+1)
!
    else if (coli.eq.'I') then
        if (interp(1:3) .eq. 'NON') then
            i1 = 1 + 2 * ( i - 1 )
            i2 = 1 + 2 * i
            tole = epsi * abs( zr(lvale+i-1) - zr(lvale+i) )
            if (abs(zr(lvale+i-1)-valr) .le. tole) then
                resure = zr(lfonc+i1)
                resuim = zr(lfonc+i1+1)
            else if (abs(zr(lvale+i)-valr) .le. tole) then
                resure = zr(lfonc+i2)
                resuim = zr(lfonc+i2+1)
            else
                ier = 200
                call u2mess('A', 'UTILITAI2_16')
                goto 999
            endif
        else if (interp.eq.'LIN LIN ') then
            i1 = 1 + 2 * ( i - 1 )
            i2 = 1 + 2 * i
            resure = linlin( valr, zr(lvale+i-1), zr(lfonc+i1), zr(lvale+ i), zr(lfonc+i2) )
            resuim = linlin( valr, zr(lvale+i-1), zr(lfonc+i1+1), zr(lvale+i), zr(lfonc+i2+1) )
        else if (interp.eq.'LIN LOG ') then
            i1 = 1 + 2 * ( i - 1 )
            i2 = 1 + 2 * i
            resure = linlog( valr, zr(lvale+i-1), zr(lfonc+i1), zr(lvale+ i), zr(lfonc+i2) )
            resuim = linlog( valr, zr(lvale+i-1), zr(lfonc+i1+1), zr(lvale+i), zr(lfonc+i2+1) )
        else if (interp.eq.'LOG LOG ') then
            i1 = 1 + 2 * ( i - 1 )
            i2 = 1 + 2 * i
            resure = loglog( valr, zr(lvale+i-1), zr(lfonc+i1), zr(lvale+ i), zr(lfonc+i2) )
            resuim = loglog( valr, zr(lvale+i-1), zr(lfonc+i1+1), zr(lvale+i), zr(lfonc+i2+1) )
        else if (interp.eq.'LOG LIN ') then
            i1 = 1 + 2 * ( i - 1 )
            i2 = 1 + 2 * i
            resure = loglin( valr, zr(lvale+i-1), zr(lfonc+i1), zr(lvale+ i), zr(lfonc+i2) )
            resuim = loglin( valr, zr(lvale+i-1), zr(lfonc+i1+1), zr(lvale+i), zr(lfonc+i2+1) )
        else
            ier = 230
            call u2mesk('A', 'UTILITAI2_17', 1, interp)
            goto 999
        endif
!
    else if (coli.eq.'E') then
        i1 = 1 + 2 * ( i - 1 )
        i2 = 1 + 2 * i
        resure = linlin( valr,zr(lvale+i-1),zr(lfonc+i1), zr(lvale+i), zr(lfonc+i2))
        resuim = linlin( valr, zr(lvale+i-1), zr(lfonc+i1+1), zr(lvale+i), zr(lfonc+i2+1) )
!
    else
        ier = 240
        call u2mesk('A', 'PREPOST3_6', 1, coli)
    endif
!
999 continue
!
    if (ier .ne. 0 .and. codmes .ne. ' ') then
        call u2mesk(codmes, 'FONCT0_9', 1, nomf)
    endif
!
    call jedema()
end subroutine
