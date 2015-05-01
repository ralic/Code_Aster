subroutine fointe(codmes, nomf, nbpu, nompu, valpu,&
                  resu, ier)
    implicit none
#include "jeveux.h"
#include "asterc/fiintf.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/focoli.h"
#include "asterfort/fointn.h"
#include "asterfort/folocx.h"
#include "asterfort/fonbpa.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    character(len=*), intent(in) :: codmes
    character(len=*), intent(in) :: nomf
    integer, intent(in) :: nbpu
    character(len=*), intent(in) :: nompu(*)
    real(kind=8), intent(in) :: valpu(*)
    real(kind=8), intent(out) :: resu
    integer, intent(out) :: ier
!     ------------------------------------------------------------------
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
!     INTERPOLATION POUR CALCULER RESU = F(X,Y,Z,...)
!     ------------------------------------------------------------------
! IN  CODMES : 'F','E','A','I',... PARAMETRE TRANSMIT A UTMESS, UTMESK
! IN  NOMF   : NOM DE LA FONCTION OU DE LA NAPPE
! IN  NBPU   : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
! IN  NOMPU  : NOMS DES PARAMETRES "UTILISATEUR"
! IN  VALPU  : VALEURS DES PARAMETRES "UTILISATEUR"
! OUT RESU   : RESULTAT DE L'INTERPOLATION
! OUT IER    : CODE RETOUR
!
! CODE RETOUR DE FOLOCX :
! IER = 10  : MOINS DE 1 POINT
! IER = 20  : EXTRAPOLATION INCONNUE
! IER = 30  : ON DEBORDE A GAUCHE
! IER = 40  : ON DEBORDE A DROITE
!
! CODE RETOUR DE FOCOLI :
! IER = 200 : INTERPOLATION DE LA FONCTION NON PERMISE
! IER = 210 : PARAMETRE EN DOUBLE
! IER = 220 : PARAMETRE ATTENDUS,PARAMETRES RECUS
! IER = 230 : TYPE D'INTERPOLATION DE LA FONCTION INCONNU
! IER = 240 : RECHERCHE DE LA VALEUR INCONNUE (COLI)
!
! CODE RETOUR DE FOINTE :
! IER = 100 : TYPE DE FONCTION NON VALIDE
! IER = 110 : PAS ASSEZ DE PARAMETRES
! IER = 120 : PARAMETRE EN DOUBLE
! IER = 130 : PARAMETRE ATTENDUS,PARAMETRES RECUS
! IER = 140 : TYPE D'INTERPOLATION SUR LES PARA DE LA NAPPE INCONNU
! IER = 150 : TYPE DE FONCTION NON TRAITE
! IER = 160 : PAS ASSEZ DE PARAMETRES
! IER = 170 : INTERPOLATION SUR LES PARAMETRES DE LA NAPPE NON PERMISE
! IER = 200 : ERREUR AVEC UNE FORMULE
!     ------------------------------------------------------------------
!
    integer :: nupar, i, nbvn, izero, isave, lprol, lvar, nbpt, lpara
    integer :: iret, lfon, iadzi, iazk24
    character(len=1) :: coli, cbid, xous
    character(len=2) :: codme2
    character(len=8) :: nomail
    character(len=19) :: nomfon
    character(len=24) :: chprol, chvale, chpara
    real(kind=8) :: rvar, rpar, tab(4), epsi
!     ------------------------------------------------------------------
    character(len=24) :: valk(3)
    integer :: vali(2)
!     ------------------------------------------------------------------
    integer :: mxsave, mxpara, svnbpa, svpar, isvnxt, isvind, nextsv
    integer :: iaprol, iavale, iapara, luvale, lupara
    real(kind=8) :: svresu
    character(len=1) :: svtypf
    character(len=2) :: svprgd
    character(len=24) :: svinte
    character(len=16) :: svnomp
    character(len=19) :: svnomf
    common /ifosav/ mxsave, mxpara, svnbpa(4) , svpar(10,4) ,&
     &                isvnxt , isvind(4), nextsv(4)
    common /jfosav/ iaprol(4),iavale(4),iapara(4),luvale(4),lupara(4)
    common /rfosav/ svresu(4)
    common /kfosav/ svnomp(10,4) , svnomf(4) ,&
     &                svtypf(4) , svprgd(4) , svinte(4)
!
!     ------------------------------------------------------------------
!     FONCTION EN LIGNE
!
#define linlin(x,x1,y1,x2,y2) y1+(x-x1)*(y2-y1)/(x2-x1)
#define linlog(x,x1,y1,x2,y2) exp(log(y1)+(x-x1)*(log(y2)-log(y1)) \
    /(x2-x1))
#define loglog(x,x1,y1,x2,y2) exp(log(y1)+(log(x)-log(x1))*(log(y2) \
    -log(y1))/(log(x2)-log(x1)))
#define loglin(x,x1,y1,x2,y2) y1+(log(x)-log(x1))*(y2-y1) \
    /(log(x2)-log(x1))
!     ------------------------------------------------------------------
    call jemarq()
!
    codme2 = codmes
    epsi = sqrt ( r8prem() )
    ier = 0
    izero = 0
    nomfon = nomf
    resu = r8vide()
!
    chprol = nomfon//'.PROL'
    chvale = nomfon//'.VALE'
    chpara = nomfon//'.PARA'
!
    do i = 1, mxsave
        if (nomfon .eq. svnomf(i)) then
            isave = i
            lprol=iaprol(isave)
            lvar =iavale(isave)
            nbpt =luvale(isave)
            lpara=iapara(isave)
            nbvn=lupara(isave)
            goto 11
        endif
    end do
!
!
    call jeveut(chprol, 'L', lprol)
    if (zk24(lprol) .eq. 'INTERPRE') then
!     ------------------------ CAS DES FORMULES ------------------------
        call fiintf(nomf, nbpu, nompu, valpu, ier,&
                    'A', resu)
        if (ier .gt. 0) then
            ier = 200
        endif
        goto 999
    endif
!
!
!     --- MEMORISATION DES INFORMATIONS NOUVELLES ---
    isvnxt = nextsv(isvnxt)
    isave = isvnxt
!
    call jeveut(chvale, 'L', lvar)
!     -- SI L'OBJET .VALE EST UN OBJET SIMPLE, ON STOCKE 'LONUTI'
    call jelira(chvale, 'XOUS', cval=xous)
    if (xous .eq. 'S') then
        call jelira(chvale, 'LONUTI', nbpt)
    else
        nbpt=0
    endif
    call jeexin(chpara, iret)
    if (iret .gt. 0) then
        call jeveut(chpara, 'L', lpara)
        call jelira(chpara, 'LONUTI', nbvn)
    else
        lpara=0
        nbvn=0
    endif
!
    iaprol(isave) = lprol
    iavale(isave) = lvar
    luvale(isave) = nbpt
    iapara(isave) = lpara
    lupara(isave) = nbvn
!
    svtypf(isave) = zk24(lprol)(1:1)
    svinte(isave) = zk24(lprol+1)
    svprgd(isave) = zk24(lprol+4)(1:2)
!
 11 continue
!
!     --- CAS PARTICULIER DES CONSTANTES ---
    if (svtypf(isave) .eq. 'C') then
        if (nomfon .ne. svnomf(isave)) then
            lvar=iavale(isave)
            svresu(isave) = zr(lvar+1)
            svnomf(isave) = nomfon
        endif
        resu = svresu(isave)
        goto 9998
    endif
!
!
!     --- VERIFICATION DE LA VALIDITE DES PARAMETRES ----
    if (nomfon .eq. svnomf(isave)) then
        if (nbpu .eq. svnbpa(isave)) then
            do i = 1, svnbpa(isave)
                if (nompu(i) .ne. svnomp(i,isave)) then
                    goto 19
                else
                    svpar(i,isave)=i
                endif
            end do
!           --- SI SUCCES ALORS ON SAUTE LES VERIFICATIONS ----
            goto 30
        endif
    endif
!
!     --- SI ECHEC PRECEDENT ALORS ON VERIFIE ---
 19 continue
    call fonbpa(nomfon, zk24(lprol), cbid, mxpara, svnbpa(isave),&
                svnomp(1, isave))
    if (nbpu .lt. svnbpa(isave)) then
        ier = 160
        vali(1)=nbpu
        vali(2)=svnbpa(isave)
        call utmess('A+', 'FONCT0_9', sk=nomfon)
        call utmess('A', 'FONCT0_14', ni=2, vali=vali)
        goto 9998
    endif
    do i = 1, svnbpa(isave)
        svpar(i,isave)=0
        do nupar = 1, nbpu
            if (nompu(nupar) .eq. svnomp(i,isave)) then
                if (svpar(i,isave) .eq. 0) then
                    svpar(i,isave)=nupar
                else
                    ier = 120
                    call utmess('A+', 'FONCT0_9', sk=nomfon)
                    call utmess('A', 'FONCT0_15', nk=nbpu, valk=nompu)
                    goto 9998
                endif
            endif
        end do
        if (svpar(i,isave) .eq. 0) then
            ier = 130
            call utmess('A+', 'FONCT0_9', sk=nomfon)
            call utmess('A+', 'FONCT0_16', nk=svnbpa(isave), valk=svnomp(1, isave))
            call utmess('A', 'FONCT0_17', nk=nbpu, valk=nompu)
            goto 9998
        endif
    end do
!
!     ------------------------ INTERPOLATION --------------------------
 30 continue
!
    if (svtypf(isave) .eq. 'F') then
!
!        --- FONCTION ---
        lvar=iavale(isave)
        nbpt=luvale(isave)
        nbpt = nbpt/2
        lfon = lvar + nbpt
        rvar = valpu(svpar(1,isave))
        call folocx(zr(lvar), nbpt, rvar, svprgd(isave), isvind(isave),&
                    epsi, coli, ier)
        if (ier .ne. 0) goto 9998
        call focoli(isvind(isave), coli, svinte(isave), zr(lvar), zr(lfon),&
                    rvar, resu, ier)
        if (ier .ne. 0) goto 9998
        svresu(isave) = resu
!
!     --- NAPPE ---
!
    else if (svtypf(isave) .eq. 'N') then
        rpar = valpu(svpar(1,isave))
        rvar = valpu(svpar(2,isave))
        lpara=iapara(isave)
        nbvn=lupara(isave)
        i = 1
        call folocx(zr(lpara), nbvn, rpar, svprgd(isave), i,&
                    epsi, coli, ier)
        if (ier .ne. 0) goto 9998
!
        if (coli .eq. 'C') then
            call fointn(izero, nomf, rvar, i, epsi,&
                        resu, ier)
            if (ier .ne. 0) goto 9998
        else if (coli.eq.'I') then
            if (svinte(isave)(1:3) .eq. 'NON') then
                call utmess('A', 'FONCT0_11', sk=nomfon)
                ier = 170
                goto 9998
            endif
            call fointn(izero, nomf, rvar, i, epsi,&
                        tab(3), ier)
            if (ier .ne. 0) goto 9998
            call fointn(izero, nomf, rvar, i+1, epsi,&
                        tab(4), ier)
            if (ier .ne. 0) goto 9998
!
!           --- INTERPOLATION FINALE SUR LES PARAMETRES ---
            tab(1) = zr(lpara+i-1)
            tab(2) = zr(lpara+i )
            if (svinte(isave) .eq. 'LIN LIN ') then
                resu = linlin(rpar,tab(1),tab(3),tab(2),tab(4))
            else if (svinte(isave).eq.'LIN LOG ') then
                resu = linlog(rpar,tab(1),tab(3),tab(2),tab(4))
            else if (svinte(isave).eq.'LOG LOG ') then
                resu = loglog(rpar,tab(1),tab(3),tab(2),tab(4))
            else if (svinte(isave).eq.'LOG LIN ') then
                resu = loglin(rpar,tab(1),tab(3),tab(2),tab(4))
            endif
        else if (coli.eq.'E') then
            call fointn(izero, nomf, rvar, i, epsi,&
                        tab(3), ier)
            if (ier .ne. 0) goto 9998
            call fointn(izero, nomf, rvar, i+1, epsi,&
                        tab(4), ier)
            if (ier .ne. 0) goto 9998
            tab(1) = zr(lpara+i-1)
            tab(2) = zr(lpara+i )
            resu = linlin(rpar,tab(1),tab(3),tab(2),tab(4))
        else
            call utmess('A+', 'FONCT0_9', sk=nomfon)
            call utmess('A', 'FONCT0_12', sr=rvar)
            ier = 140
            goto 9998
        endif
!
    else
        valk(1)=nomfon
        valk(2)=svtypf(isave)
        valk(3)='FOINTE'
        call utmess('A', 'FONCT0_13', nk=3, valk=valk)
        ier = 150
        goto 9998
    endif
!
9998 continue
    svnomf(isave) = nomfon
999 continue
!
    if (ier .ne. 0) then
        if (codme2(1:1) .ne. ' ') then
!          SI ON A L'INFO, ON AFFICHERA LA MAILLE CONCERNEE
            if (codme2(2:2) .eq. 'M') then
                call tecael(iadzi, iazk24)
                nomail = zk24(iazk24-1+3)(1:8)
                codme2(2:2) = '+'
            else
                codme2(2:2) = ' '
            endif
            call utmess(codme2(1:1)//'+', 'FONCT0_9', sk=nomfon)
            call utmess(codme2, 'FONCT0_54', nk=nbpu, valk=nompu, si=nbpu)
            if (codme2(2:2) .eq. '+') then
                call utmess(codme2(1:1), 'FONCT0_10', sk=nomail)
            endif
        endif
    endif
!
    call jedema()
end subroutine
