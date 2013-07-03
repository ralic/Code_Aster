subroutine iradhs(versio)
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
!                BUT: TRAITER LES "ADHERENCES SUPERTAB":
!     OBJETS JEVEUX CREES:
!        &&IRADHS.CODEGRA : TABLEAU D'ENTIERS DIMENSIONNE
!                 AU NOMBRE DE TYPE_MAILLES DONNANT LE CODE GRAPHIQUE
!                 DE CHAQUE TYPE_MAILLE POUR SUPERTAB.
!        &&IRADHS.CODEPHY : TABLEAU D'ENTIERS DIMENSIONNE
!                 AU NOMBRE DE TYPE_ELEM DONNANT LE CODE PHYSIQUE
!                 DE CHAQUE TYPE_ELEMENT POUR SUPERTAB.
!        &&IRADHS.CODEPHD : TABLEAU D'ENTIERS DIMENSIONNE
!                 AU NOMBRE DE TYPE_MAILLE DONNANT LE CODE PHYSIQUE
!                 PAR DEFAUT DE CHAQUE TYPE_MAILLE POUR SUPERTAB.
!        &&IRADHS.PERMUTA : TABLEAU D'ENTIERS DIMENSIONNE
!                 AU NOMBRE DE TYPE_MAILLES*NOMBRE MAXI DE NOEUDS/MAILLE
!                 DONNANT LES PERMUTATIONS EVENTUELLES DANS L'ORDRE DES
!                 NOEUDS DE CHAQUE TYPE_MAILLE, ENTRE ASTER ET SUPERTAB.
!                 LE NOMBRE MAXI DE NOEUDS/MAILLE EST STOCKE AU BOUT DU
!                 VECTEUR
!
!   IN:  VERSIO = VERSION D'IDEAS 4 OU 5(DEFAUT)
!
#include "jeveux.h"
!
#include "asterfort/inistb.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utidea.h"
#include "asterfort/wkvect.h"
    character(len=2) :: axdpcp(4)
    character(len=5) :: phe(2), mot
    integer :: versio
    character(len=8) :: nommai
    character(len=16) :: nomele
!
!-----------------------------------------------------------------------
    integer :: ia1, iax, iel, ima, imper, ino, inos
    integer :: iphe, iret1, iret2, iret3, iret4, iret5, iret6
    integer :: isu, itel, jcod1, jcod2, jcodd, jnbnoe, jpefsu
    integer :: jpermu, jpersu, maxfa, maxnod, nbn, nbtyel, nbtyma
    integer :: nbtyms, ntymax
!-----------------------------------------------------------------------
    parameter (maxnod=32,ntymax=69,maxfa=6)
    character(len=8) :: nomail(ntymax), nomtm
    integer :: limail(ntymax), indic(ntymax), indicf(ntymax), icas
    character(len=1) :: k1bid
!
    data axdpcp/'AX','DP','CP','PL'/
!
    call jemarq()
    call jeexin('&&IRADHS.PERMSUP', iret1)
    call jeexin('&&IRADHS.PERFSUP', iret2)
    call jeexin('&&IRADHS.CODEGRA', iret3)
    call jeexin('&&IRADHS.PERMUTA', iret4)
    call jeexin('&&IRADHS.CODEPHY', iret5)
    call jeexin('&&IRADHS.CODEPHD', iret6)
    if (iret1*iret2*iret3*iret4*iret5*iret6 .ne. 0) goto 9999
!
    if (iret1 .eq. 0) then
        call wkvect('&&IRADHS.PERMSUP', 'V V I', maxnod*ntymax, jpersu)
    endif
    call jeveuo('&&IRADHS.PERMSUP', 'E', jpersu)
    if (iret2 .eq. 0) then
        call wkvect('&&IRADHS.PERFSUP', 'V V I', maxfa*ntymax, jpefsu)
    endif
    call jeveuo('&&IRADHS.PERFSUP', 'E', jpefsu)
    call inistb(maxnod, nbtyms, nomail, indic, zi(jpersu),&
                limail, indicf, zi(jpefsu), maxfa)
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtyma, k1bid)
    if (iret3 .eq. 0) then
        call jecreo('&&IRADHS.CODEGRA', 'V V I')
        call jeecra('&&IRADHS.CODEGRA', 'LONMAX', nbtyma, ' ')
    endif
    call jeveuo('&&IRADHS.CODEGRA', 'E', jcod1)
    do 1 ima = 1, nbtyma
        call jenuno(jexnum('&CATA.TM.NOMTM', ima), nomtm)
        if (nomtm .eq. 'HEXA27') nomtm = 'HEXA20'
        if (nomtm .eq. 'PENTA18') nomtm = 'PENTA15'
        if (nomtm .eq. 'TRIA7') nomtm = 'TRIA6'
        if (nomtm .eq. 'QUAD9') nomtm = 'QUAD8'
        if (nomtm .eq. 'SEG4') nomtm = 'SEG2'
        do 2 isu = 1, nbtyms
            if (nomtm .eq. nomail(isu)) then
                zi(jcod1-1+ima)=isu
                goto 1
            endif
 2      continue
 1  end do
    if (iret4 .eq. 0) then
        call wkvect('&&IRADHS.PERMUTA', 'V V I', maxnod*ntymax+1, jpermu)
        zi(jpermu-1+maxnod*ntymax+1)=maxnod
    endif
    if (iret6 .eq. 0) then
        call wkvect('&&IRADHS.CODEPHD', 'V V I', nbtyma, jcodd)
    endif
    call jeveuo('&&IRADHS.PERMUTA', 'E', jpermu)
    call jeveuo('&CATA.TM.NBNO', 'L', jnbnoe)
    do 4 ima = 1, nbtyma
        nbn=zi(jnbnoe-1+ima)
        isu=zi(jcod1-1+ima)
        if (isu .eq. 0) then
            icas = 0
        else
            icas = indic(isu)
        endif
!
        if (icas .lt. 0) then
            do 41 ino = 1, nbn
                zi(jpermu-1+maxnod*(ima-1)+ino)=0
41          continue
        else if (icas.eq.0) then
            do 42 ino = 1, nbn
                zi(jpermu-1+maxnod*(ima-1)+ino)=ino
42          continue
        else
            do 43 ino = 1, nbn
                do 44 inos = 1, nbn
                    imper=zi(jpersu-1+maxnod*(isu-1)+inos)
                    if (ino .eq. imper) then
                        zi(jpermu-1+maxnod*(ima-1)+ino)=inos
                        goto 43
                    endif
44              continue
43          continue
        endif
        call jenuno(jexnum('&CATA.TM.NOMTM', ima), nommai)
        call utidea(nommai, zi(jcodd-1+ima), versio)
 4  end do
    call jelira('&CATA.TE.NOMTE', 'NOMMAX', nbtyel, k1bid)
    if (iret5 .eq. 0) then
        call jecreo('&&IRADHS.CODEPHY', 'V V I')
        call jeecra('&&IRADHS.CODEPHY', 'LONMAX', nbtyel, ' ')
        call jeveuo('&CATA.TE.TYPEMA', 'L', ia1)
    endif
    call jeveuo('&&IRADHS.CODEPHY', 'E', jcod2)
    do 55 itel = 1, nbtyel
        call jenuno(jexnum('&CATA.TE.NOMTE', itel), nomele)
        call utidea(zk8(ia1-1+itel), zi(jcod2-1+itel), versio)
55  end do
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDKQU4'), iel)
    if (iel .ne. 0) zi(jcod2-1+iel)=94
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDKTR3'), iel)
    if (iel .ne. 0) zi(jcod2-1+iel)=91
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDSQU4'), iel)
    if (iel .ne. 0) zi(jcod2-1+iel)=94
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEDSTR3'), iel)
    if (iel .ne. 0) zi(jcod2-1+iel)=91
    call jenonu(jexnom('&CATA.TE.NOMTE', 'MEQ4QU4'), iel)
    if (iel .ne. 0) zi(jcod2-1+iel)=94
    phe(1)='MECA_'
    phe(2)='THER_'
    do 3 iphe = 1, 2
!
        do 5 iax = 1, 4
            mot=phe(iphe)(1:2)//axdpcp(iax)
            call jenonu(jexnom('&CATA.TE.NOMTE', mot(1:4)//'QU4'), iel)
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'AX') zi(jcod2-1+iel)=84
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'CP') zi(jcod2-1+iel)=44
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'DP') zi(jcod2-1+iel)=54
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'PL') zi(jcod2-1+iel)=44
            call jenonu(jexnom('&CATA.TE.NOMTE', mot(1:4)//'QU8'), iel)
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'AX') zi(jcod2-1+iel)=85
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'CP') zi(jcod2-1+iel)=45
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'DP') zi(jcod2-1+iel)=55
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'PL') zi(jcod2-1+iel)=45
            call jenonu(jexnom('&CATA.TE.NOMTE', mot(1:4)//'TR3'), iel)
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'AX') zi(jcod2-1+iel)=81
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'CP') zi(jcod2-1+iel)=41
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'DP') zi(jcod2-1+iel)=51
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'PL') zi(jcod2-1+iel)=41
            call jenonu(jexnom('&CATA.TE.NOMTE', mot(1:4)//'TR6'), iel)
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'AX') zi(jcod2-1+iel)=82
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'CP') zi(jcod2-1+iel)=42
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'DP') zi(jcod2-1+iel)=52
            if (iel .ne. 0 .and. axdpcp(iax) .eq. 'PL') zi(jcod2-1+iel)=42
            call jenonu(jexnom('&CATA.TE.NOMTE', mot(1:4)//'SE2'), iel)
            if (iel .ne. 0) zi(jcod2-1+iel)=21
 5      continue
 3  end do
    call jedetr('&&IRADHS.PERMSUP')
    call jedetr('&&IRADHS.PERFSUP')
9999  continue
    call jedema()
end subroutine
