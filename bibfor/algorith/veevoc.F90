subroutine veevoc(nomo, mate, carele, varplu, lischa,&
                  partps, vecele)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lisdef.h"
#include "asterfort/lisico.h"
#include "asterfort/lislco.h"
#include "asterfort/lisllc.h"
#include "asterfort/lisnbg.h"
#include "asterfort/lisnnb.h"
#include "asterfort/vechmp.h"
#include "asterfort/vechms.h"
#include "asterfort/veevop.h"
    character(len=24) :: mate, carele
    character(len=8) :: nomo
    real(kind=8) :: partps(3)
    character(len=19) :: lischa, varplu
    character(len=19) :: vecele
!
! ----------------------------------------------------------------------
!
! CALCUL DES VECTEURS ELEMENTAIRES DES CHARGEMENTS MECANIQUES
! DE EVOL_CHAR
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  LISCHA : SD LISTE DES CHARGES
! IN  PARTPS : TABLEAU DONNANT T+, DELTAT ET THETA (POUR LE THM)
! IN  CARELE : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE   : MATERIAU CODE
! IN  VARPLU : VARIABLES DE COMMANDE A L'INSTANT T+
! OUT VECELE : VECT_ELEM RESULTAT
!
! ----------------------------------------------------------------------
!
    integer :: nbin
    parameter    (nbin=42)
    character(len=8) :: lpain(nbin)
    character(len=19) :: lchin(nbin)
!
    character(len=19) :: nomobj, lisch2
    character(len=13) :: prefob
    integer :: nbevoc, lastin
    integer :: ichar, nbchar, itypob
    logical :: levoc, lxfem
    integer :: codcha
    character(len=8) :: k8bid
    integer :: ibid, iposit, ier
    character(len=8) :: fnocal
    integer :: jfnoe
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    instan = partps(1)
    call exixfe(nomo, ier)
    lxfem = ier.ne.0
!
! --- NOMBRE DE CHARGES
!
    call lisnnb(lischa, nbchar)
!
! --- NOMBRE DE CHARGES DE TYPE EVOL_CHAR
!
    nbevoc = lisnbg(lischa,'EVOL_CHAR')
    if (nbevoc .eq. 0) goto 99
!
! --- CHAMPS D'ENTREES STANDARDS
!
    call vechmp(nomo, mate, carele, varplu, lxfem,&
                partps, nbin, lpain, lchin, lastin)
!
! --- BOUCLE SUR LES CHARGES
!
    do 10 ichar = 1, nbchar
!
! ----- CODE DU GENRE DE LA CHARGE
!
        call lislco(lischa, ichar, codcha)
        levoc = lisico('EVOL_CHAR',codcha)
        if (levoc) then
!
! ------- PREFIXE DE L'OBJET DE LA CHARGE
!
            call lisllc(lischa, ichar, prefob)
!
! ------- NOM OBJET
!
            call lisdef('POEC', 'EVOL_CHAR', ibid, k8bid, iposit)
            call lisdef('OBJE', prefob, iposit, nomobj, itypob)
!
! ------- ON N'ATTEND PAS UNE CARTE !
!
            call assert(itypob.ne.1)
            call jeexin(nomobj, ier)
            call assert(ier.ne.0)
!
! ------- NOM DE LA SD EVOL_CHAR
!
            call jeveuo(nomobj//'CHAR', 'L', jfnoe)
            fnocal = zk8(jfnoe)
!
! ------- PREPARATION LISTE DES CHARGES EFFECTIVES
!
            lisch2 = '&&VEEVOC.LISCHA'
            call veevop(nomo, fnocal, instan, lisch2)
!
! ------- CALCUL NEUMANN STANDARD
!
            call vechms(nomo, mate, carele, varplu, lischa,&
                        partps, vecele)
!
            call jedetr(lisch2)
        endif
10  end do
!
99  continue
!
    call jedema()
end subroutine
