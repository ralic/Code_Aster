subroutine accept(f, nbm, method, imode, jmode,&
                  uflui, jc, dir, uc, uct,&
                  l, lt)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     OPERATEUR PROJ_SPEC_BASE
!     PROJECTION D UN OU PLUSIEURS SPECTRES DE TURBULENCE SUR UNE BASE
!     MODALE PERTURBEE PAR PRISE EN COMPTE DU COUPLAGE FLUIDE STRUCTURE
!-----------------------------------------------------------------------
!  IN   : IEX = 0 => ON DEMANDE L EXECUTION DE LA COMMANDE
!         IEX = 1 => ON NE FAIT QUE VERIFIER LES PRECONDITIONS
!  OUT  : IER = 0 => TOUT S EST BIEN PASSE
!         IER > 0 => NOMBRE D ERREURS RENCONTREES
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/coegen.h"
#include "asterfort/corcos.h"
#include "asterfort/coyang.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: nbm, i, ind, jnd, ipg, jpg, iad1, iad2, itab, imode, jmode
    integer :: ispe, jspe, iorig, jorig, ntail, ntail1, ntail2
    integer :: jpgini, jpgfin
    real(kind=8) :: f, pi, deuxpi, omega, uc, kl, kt, dir(3, 3)
    real(kind=8) :: d1, d2, d3, mes(3), coeh, jc, l, local(3), uflui
    real(kind=8) :: uct, lt, rayon, dist, dteta, jc1
    character(len=8) :: method
!
    parameter (pi=3.14159265d0)
    data     (local(i),i=1,3) /3*0.d0/
!
!
!-----------------------------------------------------------------------
    call jemarq()
!
! QUELQUES CONSTANTES
    deuxpi=2*pi
    omega=deuxpi*f
    if (method(1:7) .ne. 'AU_YANG') uc=0.65d0*uflui
    kl = 0.1d0*omega/uc
    kt = 0.55d0*omega/uc
!
! BOUCLE SUR LES ELEMENTS DU MODELE
    jc=0.d0
    jc1=0.d0
    call jeveuo('&&GROTAB.TAB', 'L', itab)
    call jelira('&&GROTAB.TAB', 'LONUTI', ntail)
    if (method(1:7) .eq. 'AU_YANG') rayon = zr(itab+ntail-1)
    ntail = ntail - 1
    ntail1=ntail/(6*nbm)
    ntail2=ntail/nbm
    iorig=(imode-1)*ntail2
    jorig=(jmode-1)*ntail2
    jpgfin=(jmode-1)*ntail2+ntail1-1
    do 203 ipg = (imode-1)*ntail2, (imode-1)*ntail2+ntail1-1
        if (jmode .eq. imode) then
            jpgini = ipg
        else
            jpgini = (jmode-1)*ntail
        endif
        ispe=ipg-(imode-1)*ntail2
        iad1=itab + iorig + 6*ispe
        do 204 jpg = jpgini, jpgfin
            jspe=jpg-(jmode-1)*ntail2
            iad2=itab + jorig + 6*jspe
! CALCUL DISTANCES INTER POINTS DE GAUSS
! ABSCISSES
            mes(1)=zr(iad1+1)-zr(iad2+1)
! ORDONNEES
            mes(2)=zr(iad1+2)-zr(iad2+2)
!
! COTE
            mes(3)=zr(iad1+3)-zr(iad2+3)
!
! COHERENCE CORCOS
            if (method(1:6) .eq. 'CORCOS') then
                do 205 ind = 1, 3
                    local(ind)=0.d0
                    do 206 jnd = 1, 3
                        local(ind)=local(ind)+dir(ind,jnd)*mes(jnd)
206                  continue
205              continue
!
                d1=abs(local(1))
                d2=abs(local(2))
                d3=abs(local(3))
!
                coeh=corcos(d1,d2,local(1),uc,kt,kl,omega)
!
! COHERENCE GENERALE
            else if (method.eq.'GENERALE') then
                d1=abs(mes(1))
                d2=abs(mes(2))
                d3=abs(mes(3))
                coeh=coegen(d1,d2,d3,l,omega,uc)
            else if (method(1:7).eq.'AU_YANG') then
                dist = abs(zr(iad1+4)-zr(iad2+4))
                dteta= abs(zr(iad1+5)-zr(iad2+5))
                coeh = coyang (dist,dteta,rayon,omega,uc,uct,l,lt)
            endif
            if (jmode .eq. imode .and. jpg .gt. ipg) then
                jc1 = jc1 + coeh*zr(iad1)*zr(iad2)
            else
                jc = jc + coeh*zr(iad1)*zr(iad2)
            endif
204      continue
203  continue
    if (imode .eq. jmode) jc = jc + 2*jc1
!
    call jedema()
end subroutine
