subroutine noligr(ligrz, igrel, numel, nb, li,&
                  lk, code, irepe, inema, nbno,&
                  typlaz)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/poslag.h"
#include "asterfort/u2mesg.h"
    character(len=*) :: ligrz, lk(*), typlaz
    character(len=8) :: typlag
    character(len=19) :: ligr
    integer :: igrel, numel, nb, li(*), code, irepe, inema, nbno(*)
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
!
!     BUT: REMPLIR LIGR
!      1.  ADJONCTION DU GREL DANS LE LIGR
!      2.  STOCKAGE DES MAILLES ET NOEUDS SUPPLEMENTAIRES:
!        2.1 STOCKAGE DES MAILLES SUPPLEMENTAIRES DANS .NEMA
!        2.2 STOCKAGE DES NUMEROS DES MAILLES SUPPLEMENTAIRES DANS .LIEL
!
! ARGUMENTS D'ENTREE:
!      LIGR : NOM DU LIGREL
!      IGREL: NUMERO DU GREL
!      NUMEL:NUMERO DU TYPE_ELEMENT
!      NB   :NOMBRE DE NOEUDS DE LA LISTE
!      LI   :LISTE DE NUMEROS DE NOEUDS (AU NOMBRE DE NB)
!      LK   :LISTE DE NOMS DE NOEUDS (AU NOMBRE DE -NB)
!      CODE : 1 ==> UNE MAILLE "POI1" PAR NOEUD
!                   (TYPIQUEMENT: FORCE_NODALE)
!           : 2 ==> UNE MAILLE "SEG2" ET UN NOEUD TARDIF PAR NOEUD
!                   (TYPIQUEMENT: DDL_IMPO    )
!           : 3 ==> UNE MAILLE "SEG2" PAR NOEUD, ET UN SEUL NOEUD TARDIF
!                   SON NUMERO EST INCREMENTE PAR LA ROUTINE APPELANTE
!                   (TYPIQUEMENT: LIAISON_DDL )
!   ALTERATION : 2 DDL DE LAGRANGE =>
!           : 2 ==> UNE MAILLE "SEG3" ET 2 NOEUDS TARDIFS PAR NOEUD
!                   (TYPIQUEMENT: DDL_IMPO    )
!           : 3 ==> UNE MAILLE "SEG3" PAR NOEUD, ET 2 NOEUDS TARDIFS
!                   PAR LIAISON
!                   SON NUMERO EST INCREMENTE PAR LA ROUTINE APPELANTE
!                   (TYPIQUEMENT: LIAISON_DDL )
!           : 4 ==> UNE MAILLE "SEG3" PAR NOEUD, ET 2 NOEUDS TARDIFS
!                   PAR LIAISON SUR NB LIAISONS DU MEME TYPE
!                   LES NUMERO DES NOEUDS TARDIFS SONT GERES PAR LA
!                   ROUTINE APPELANTE ( POUR RELATION I NOEUD TARDIF 1
!                   DE NUM : -NBNO(I)+1 ET NOEUD TARDIF 2 -NBNO(I) )
!                   (TYPIQUEMENT: LIAISON_DDL_GROUP )
!      IREPE:NOMBRE DE REPETITIONS DE LA BOUCLE PAR NOEUD
!      INEMA:NUMERO  DE LA DERNIERE MAILLE TARDIVE DANS LIGR
!      NBNO :NUMERO  DU  DERNIER NOEUD TARDIF DANS LIGR OU LISTE DE NUME
!            RO DE NOEUDS TARDIFS(CODE 4)
!      TYPLAG:TYPE DES MULTIPLICATEURS DE LAGRANGE ASSOCIES A LA
!             RELATION
!          : '12'  ==>  LE PREMIER LAGRANGE EST AVANT LE NOEUD PHYSIQUE
!                       LE SECOND LAGRANGE EST APRES
!          : '22'  ==>  LE PREMIER LAGRANGE EST APRES LE NOEUD PHYSIQUE
!                       LE SECOND LAGRANGE EST APRES
    integer :: absnb
    integer :: vali
    character(len=24) :: liel, nema
    character(len=24) :: valk
    character(len=8) :: noma
! --- DEBUT
!-----------------------------------------------------------------------
    integer :: ic, ilag1, ilag2, jdlgns, jligr, jnema, jnoma
    integer :: k, kligr, lonigr, numpoi, numseg, nunoeu
!-----------------------------------------------------------------------
    call jemarq()
    typlag = typlaz
    call poslag(typlag, ilag1, ilag2)
    ligr=ligrz
    liel=ligr//'.LIEL'
    nema=ligr//'.NEMA'
    absnb=abs(nb)
    if (code .eq. 1) then
        call jenonu(jexnom('&CATA.TM.NBNO', 'POI1'), numpoi)
    else if (code.eq.2) then
        call jenonu(jexnom('&CATA.TM.NBNO', 'SEG3'), numseg)
    else if ((code.eq.3).or.(code.eq.4)) then
        call jenonu(jexnom('&CATA.TM.NBNO', 'SEG3'), numseg)
    else
        vali = code
        valk = 'EST INCONNU '
        call u2mesg('F', 'MODELISA8_69', 1, valk, 1,&
                    vali, 0, 0.d0)
    endif
    lonigr = absnb*irepe + 1
    call jecroc(jexnum(liel, igrel))
    call jeecra(jexnum(liel, igrel), 'LONMAX', lonigr)
    call jeveuo(jexnum(liel, igrel), 'E', jligr)
    call jeveuo(ligr//'.LGNS', 'E', jdlgns)
    call jeveuo(ligr//'.LGRF', 'E', jnoma)
    noma=zk8(jnoma)
    kligr = 0
    do 130 ic = 1, irepe
        do 110 k = 1, absnb
            if (nb .lt. 0) then
                call jenonu(jexnom(noma//'.NOMNOE', lk(k)), nunoeu)
            else
                nunoeu=li(k)
            endif
            if (code .eq. 1) then
                inema = inema + 1
                call jecroc(jexnum(nema, inema))
                call jeecra(jexnum(nema, inema), 'LONMAX', 2)
                call jeveuo(jexnum(nema, inema), 'E', jnema)
                zi(jnema-1+1) = nunoeu
                zi(jnema-1+2) = numpoi
                kligr = kligr + 1
                zi(jligr-1+kligr) = -inema
            else if (code.eq.2) then
                inema = inema + 1
                call jecroc(jexnum(nema, inema))
                call jeecra(jexnum(nema, inema), 'LONMAX', 4)
                call jeveuo(jexnum(nema, inema), 'E', jnema)
                zi(jnema-1+1) = nunoeu
                nbno(1) = nbno(1) + 1
                zi(jnema-1+2) = -nbno(1)
                nbno(1) = nbno(1) + 1
                zi(jnema-1+3) = -nbno(1)
                zi(jnema-1+4) = numseg
                kligr = kligr + 1
                zi(jligr-1+kligr) = -inema
            else if (code.eq.3) then
                inema = inema + 1
                call jecroc(jexnum(nema, inema))
                call jeecra(jexnum(nema, inema), 'LONMAX', 4)
                call jeveuo(jexnum(nema, inema), 'E', jnema)
                zi(jnema-1+1) = nunoeu
                zi(jnema-1+2) = -nbno(1)+1
                zi(jnema-1+3) = -nbno(1)
                zi(jnema-1+4) = numseg
                kligr = kligr + 1
                zi(jligr-1+kligr) = -inema
                zi(jdlgns+nbno(1)-2) = ilag1
                zi(jdlgns+nbno(1)-1) = ilag2
            else if (code.eq.4) then
                inema = inema + 1
                call jecroc(jexnum(nema, inema))
                call jeecra(jexnum(nema, inema), 'LONMAX', 4)
                call jeveuo(jexnum(nema, inema), 'E', jnema)
                zi(jnema-1+1) = nunoeu
                zi(jnema-1+2) = -nbno(k)+1
                zi(jnema-1+3) = -nbno(k)
                zi(jnema-1+4) = numseg
                kligr = kligr + 1
                zi(jligr-1+kligr) = -inema
            endif
110      continue
130  end do
    zi(jligr-1+kligr+1) = numel
    call jedema()
end subroutine
