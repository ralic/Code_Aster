subroutine erglth(champ, inst, niveau, iordr, resuco)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =====================================================================
! ERREUR GLOBALE AU MAILLAGE - THERMIQUE
! **     **                    **
! =====================================================================
!     BUT :  EN THERMIQUE, CALCULER LES ESTIMATEURS GLOBAUX
!            A PARTIR DES ESTIMATEURS LOCAUX CONTENUS DANS CHAMP
!
! IN  CHAMP    :  NOM DU CHAM_ELEM_ERREUR
! IN  INSTANT  :  INSTANT DE CALCUL
! IN  NIVEAU   :  NIVEAU DE L'ESTIMATEUR
! IN  IORDR    :  NUMERO D'ORDRE
! IN  RESUCO   :  SD RESULTAT.
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       MESSAGE : INFNIV.
!       JEVEUX  : JEMARQ,JELIRA,JEVEUO,JEDEMA.
!       ASTER   : IUNIFI,CELVER.
!       ENVIMA  : R8MIEM.
!
!     FONCTIONS INTRINSEQUES:
!       ABS,SQRT.
!  -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       05/07/01 (OB) : CREATION EN S'INSPIRANT DE ERGLOB.F.
!       12/09/02 (OB) : MODIF. MSG D'ALARME DE LA DIVISION PAR ZERO.
! --------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/celver.h"
#include "asterfort/digdel.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/utmess.h"
    real(kind=8) :: inst
    integer :: niveau, iordr
    character(len=8) :: resuco
    character(len=*) :: champ
!
! ------------------------------------------------------------------
!
! DECLARATION VARIABLES LOCALES
    integer :: ifi, longt, long2, mode, j, ibid, nbgr, icoef, nel
    integer :: idecgr, k, iad, iavale, nbel
    real(kind=8) :: termvo, termsa, termfl, termec, ovfl, terms1, termf1, terme1
    real(kind=8) :: termv1, termv2, terms2, termf2, terme2, err0, nors, nu0
    character(len=4) :: docu
    character(len=19) :: champ2, ligrel
    aster_logical :: first
    integer, pointer :: celd(:) => null()
    character(len=24), pointer :: celk(:) => null()
!
! INIT.
    call jemarq()
    ovfl = r8miem()
    ifi = iunifi('RESULTAT')
    champ2 = champ
!
! ON RETROUVE LE NOM DU LIGREL:
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(champ2, 'NBVARI_CST', 'STOP', ibid)
    call celver(champ2, 'NBSPT_1', 'STOP', ibid)
    call jelira(champ2//'.CELD', 'DOCU', cval=docu)
    if (docu .ne. 'CHML') then
        call utmess('F', 'CALCULEL5_44')
    endif
    call jeveuo(champ2//'.CELK', 'L', vk24=celk)
    ligrel = celk(1)(1:19)
!
    call jeveuo(champ2//'.CELD', 'L', vi=celd)
!
!     -- ON VERIFIE LES LONGUEURS:
    first = .true.
    nbgr = nbgrel(ligrel)
    do 1 j = 1, nbgr
        mode=celd(celd(4+j) +2)
        if (mode .eq. 0) goto 1
        long2 = digdel(mode)
        icoef=max(1,celd(4))
        long2 = long2 * icoef
        if (first) then
            longt = long2
        else
            if (longt .ne. long2) then
                call utmess('F', 'CALCULEL5_45')
            endif
        endif
        first = .false.
  1 end do
!
!        -- ON CUMULE :
    call jeveuo(champ2//'.CELV', 'E', iavale)
!
    err0 = 0.d0
    nors = 0.d0
    nbel = 0
    if (niveau .eq. 2) then
        termvo = 0.d0
        termsa = 0.d0
        termfl = 0.d0
        termec = 0.d0
        termv1 = 0.d0
        terms1 = 0.d0
        termf1 = 0.d0
        terme1 = 0.d0
        termv2 = 0.d0
        terms2 = 0.d0
        termf2 = 0.d0
        terme2 = 0.d0
    endif
!
    do 2 j = 1, nbgr
        mode=celd(celd(4+j) +2)
        if (mode .eq. 0) goto 2
        nel = nbelem(ligrel,j)
        idecgr=celd(celd(4+j)+8)
        do 3 k = 1, nel
            iad = iavale-1+idecgr+(k-1)*longt
            err0 = err0 + zr(iad)**2
            nors = nors + zr(iad+2)**2
            if (niveau .eq. 2) then
                termvo = termvo + zr(iad+3)**2
                termv1 = termv1 + zr(iad+5)**2
                termsa = termsa + zr(iad+6)**2
                terms1 = terms1 + zr(iad+8)**2
                termfl = termfl + zr(iad+9)**2
                termf1 = termf1 + zr(iad+11)**2
                termec = termec + zr(iad+12)**2
                terme1 = terme1 + zr(iad+14)**2
            endif
            nbel = nbel + 1
  3     continue
  2 continue
    err0 = sqrt(err0)
    nors = sqrt(nors)
    if (niveau .eq. 2) then
! ERREURS PARTIELLES ABSOLUES
        termvo = sqrt(termvo)
        termsa = sqrt(termsa)
        termfl = sqrt(termfl)
        termec = sqrt(termec)
        termv1 = sqrt(termv1)
        terms1 = sqrt(terms1)
        termf1 = sqrt(termf1)
        terme1 = sqrt(terme1)
! ERREURS PARTIELLES RELATIVES
        if (termv1 .gt. ovfl) termv2 = 100.d0*(termvo/termv1)
        if (terms1 .gt. ovfl) terms2 = 100.d0*(termsa/terms1)
        if (termf1 .gt. ovfl) termf2 = 100.d0*(termfl/termf1)
        if (terme1 .gt. ovfl) terme2 = 100.d0*(termec/terme1)
    endif
    if (nors .gt. ovfl) then
        nu0 = 100.d0*err0/nors
    else
        call utmess('I', 'CALCULEL5_46')
        nu0 = 0.d0
    endif
    write(ifi,*) ' '
    write(ifi,*) '**********************************************'
    write(ifi,*) ' THERMIQUE: ESTIMATEUR D''ERREUR EN RESIDU '
    write(ifi,*) '**********************************************'
    write(ifi,*)
    write(ifi,*) '   IMPRESSION DES NORMES GLOBALES :'
    write(ifi,*)
!
! ESTIMATEURS D'ERREURS EN THERMIQUE LINEAIRE
    write(ifi,111)' SD EVOL_THER    ',resuco
    write(ifi,110)' NUMERO D''ORDRE  ',iordr
    write(ifi,109)' INSTANT         ',inst
    write(ifi,*)'ERREUR             ABSOLUE   /  RELATIVE '//&
     &      '/ NORMALISATION'
    write(ifi,108)' TOTAL           ',err0,nu0,'%',nors
    if (niveau .eq. 2) then
        write(ifi,108)' TERME VOLUMIQUE ',termvo,termv2,'%',termv1
        write(ifi,108)' TERME SAUT      ',termsa,terms2,'%',terms1
        write(ifi,108)' TERME FLUX      ',termfl,termf2,'%',termf1
        write(ifi,108)' TERME ECHANGE   ',termec,terme2,'%',terme1
    endif
    108 format(a17,d16.8,1x,d16.8,a2,1x,d16.8)
    109 format(a17,d16.8)
    110 format(a17,i5)
    111 format(a17,a8)
    write(ifi,*)
    write(ifi,*) '**********************************************'
    call jedema()
end subroutine
