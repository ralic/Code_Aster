subroutine rsexc2(i1, i2, nomsd, nomsy, iordr,&
                  chextr, option, iret)
    implicit none
!-----------------------------------------------------------------------
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
    integer :: i1, i2, icode, iret, j, nmax
!-----------------------------------------------------------------------
    parameter (nmax=10)
    character(len=15) :: noms(nmax)
    integer :: nb, iprec, iretg
    aster_logical :: alarme
    save noms,nb,iprec,alarme,iretg
    integer :: iordr
    character(len=*) :: nomsd, nomsy
    character(len=24) :: chextr
    character(len=24) :: valk(2)
    character(len=16) :: nomcmd, option
    character(len=8) :: concep
    character(len=16) :: typcon
    integer :: vali
! ----------------------------------------------------------------------
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
!      RECUPERATION DU NOM DU CHAMP-GD  CORRESPONDANT A:
!          NOMSD(IORDR,NOMSY).
!      IL S'AGIT D'UN APPEL A RSEXCH COMPLETE PAR DES VERIFICATIONS
!      NOTAMMENT SUR L'EXISTENCE DU CHAMP NOMSY
!      L'UTILISATION LA PLUS COURANTE CONSISTE A UTILISER I1=I2=1
!         DANS CE CAS, SI LE CHAMP NOMSY N'EXISTE PAS, ON EMET UN
!         MESSAGE D'ALARME ET ON NE CALCULE PAS L'OPTION
!      MAIS IL EST POSSIBLE D'EFFECTUER UNE RECHERCHE POUR PLUSIEURS
!      VALEURS DE NOMSY ET DE CONSERVER LA DERNIERE CORRECTE
!      PAR EXEMPLE POUR 3 NOMS SYMBOLIQUES DE CHAMP ON UTILISERA :
!      CALL RSEXC2(1,3,...,NOMSY1,...)
!      CALL RSEXC2(2,3,...,NOMSY2,...)
!      CALL RSEXC2(3,3,...,NOMSY3,...)
!      LA COHERENCE ENTRE LES DIFFERENTS APPELS EST VERIFIEE ET
!      UN EVENTUEL MESSAGE D'ALARME EST PRODUIT PAR LE DERNIER APPEL
!      CE MODULE EST DESTINE A ETRE APPELE PAR UN OPERATEUR
!      PAR EXEMPLE OP0058 AFIN DE PREPARER LES NOMS DE CHAMP-GD
!      AVANT L'APPEL A MECALC
! ----------------------------------------------------------------------
! IN  : I1,I2  : INDICE COURANT, INDICE MAXIMUM
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : NOMSY  : NOM SYMBOLIQUE DU CHAMP A CHERCHER.
! IN  : IORDR  : NUMERO D'ORDRE DU CHAMP A CHERCHER.
! OUT : CHEXTR : NOM DU CHAMP EXTRAIT.
! IN  : OPTION : NOM DE L'OPTION
! OUT : IRET   : CODE RETOUR
! ----------------------------------------------------------------------
    if (i1 .eq. 1) then
        iprec=0
        iretg=10000
    endif
!
    ASSERT((iprec.eq.0.or.nb.eq.i2).and.(iprec+1.eq.i1))
    ASSERT(i2.le.nmax)
    iprec=i1
    if (iretg .le. 0) goto 20
    noms(i1)=nomsy
    if (i1 .eq. 1) alarme=.true.
    nb=i2
    call rsexch(' ', nomsd, nomsy, iordr, chextr,&
                icode)
    alarme=alarme.and.icode.gt.0
    if (alarme .and. i1 .eq. i2) then
        call getres(concep, typcon, nomcmd)
        valk(1) = nomsd
        valk(2) = noms(1)
        call utmess('A+', 'UTILITAI8_13', nk=2, valk=valk)
        do 10 j = 2, i2
            valk(1) = noms(j)
            call utmess('A+', 'UTILITAI8_14', sk=valk(1))
 10     continue
        call utmess('A+', 'UTILITAI8_15')
        vali = iordr
        valk(1) = option
        valk(2) = nomsd
        call utmess('A', 'UTILITAI8_16', nk=2, valk=valk, si=vali)
    endif
    iretg=min(icode,iretg)
 20 continue
    iret=iretg
end subroutine
