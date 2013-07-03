subroutine nmdovd(modele, mesmai, nbma, ces2, comcod,&
                  defo)
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
    implicit none
#include "jeveux.h"
!
#include "asterc/lctest.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/teattr.h"
#include "asterfort/u2mesg.h"
    character(len=24) :: modele, mesmai
    character(len=16) :: defo, comcod
!
! PERMET DE VERIFIER SI LES DEFORMATIONS
! SONT COMPATIBLES AVEC LES ELEMENTS DU MODELE
!
! ----------------------------------------------------------------------
! IN MODELE   : LE MODELE
! IN MESMAI   : LISTE DES MAILLES AFFECTEES
! IN NBMA     : NOMRE DE CES MAILLES (0 SIGNIFIE : TOUT)
! IN CES2     :  CHAMELEM SIMPLE ISSU DE COMPOR, DEFINI SUR LES
!                ELEMENTS QUI CALCULENT FULL_MECA
! IN  COMCOD  : COMPORTEMENT PYTHON (DEFORMAITON) SUR LES MAILLES MESMAI
! IN  DEFO    : COMPORTEMENT LU ACTUELLEMENT AFFECTE AUX MAILLES MESMAI
!
!
    character(len=16) :: notype, texte(3), typmod, typmo2
    character(len=24) :: k24bid, ligrel, mailma
    character(len=19) :: ces2
    character(len=8) :: noma, nomail
    integer :: nutyel, irett, iad, nugrel
    integer :: irepe, nbma, nbmat, nbma1
    integer :: ima, i, igrel, jma, iret
    integer :: nbmagl, jcesd, jcesl, jcesv
!
!      NUMAIL(I,IEL) = ZI(IALIEL-1+ZI(ILLIEL+I-1)+IEL-1)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
!
    ligrel = modele(1:8)//'.MODELE    .LIEL'
    call jeveuo(ligrel(1:19)//'.REPE', 'L', irepe)
    call dismoi('C', 'NOM_MAILLA', modele(1:8), 'MODELE', i,&
                nomail, irett)
!
    call jeveuo(ces2//'.CESD', 'L', jcesd)
    call jeveuo(ces2//'.CESL', 'L', jcesl)
    call jeveuo(ces2//'.CESV', 'L', jcesv)
    nbmat = zi(jcesd-1+1)
!
    if (nbma .ne. 0) then
        call jeveuo(mesmai, 'L', jma)
        nbma1=nbma
    else
        nbma1=nbmat
    endif
!
    do 40,i = 1,nbma1
    if (nbma .ne. 0) then
        ima=zi(jma-1+i)
    else
        ima=i
    endif
    call cesexi('C', jcesd, jcesl, ima, 1,&
                1, 1, iad)
    if (iad .gt. 0) then
        mailma = nomail(1:8)//'.NOMMAI'
        call jenuno(jexnum(mailma, ima), noma)
!           NUMERO DU GREL CONTENANT LA MAILLE IMA
        nugrel=zi(irepe-1+2*(ima-1)+1)
        call jeveuo(jexnum(ligrel, nugrel), 'L', igrel)
        call jelira(jexnum(ligrel, nugrel), 'LONMAX', nbmagl, k24bid)
        nutyel = zi(igrel+nbmagl-1)
        call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), notype)
!
!           LECTURE DE TYPMOD DANS LE CATALOGUE PHENOMENE_MDOELISATION
        call teattr(notype, 'C', 'TYPMOD', typmod, iret)
        if (iret .ne. 0) goto 40
        call teattr(notype, 'C', 'TYPMOD2', typmo2, iret)
!
!           Dans le grel il y a TYPMOD=COMP3D
        if (typmod(1:6) .eq. 'COMP3D') then
            call lctest(comcod, 'MODELISATION', '3D', irett)
            if (irett .eq. 0) then
                texte(1)=notype
                texte(2)=noma
                texte(3)=defo
                call u2mesg('F', 'COMPOR1_52', 3, texte, 0,&
                            0, 0, 0.d0)
            endif
        else if (typmod(1:6).eq.'COMP1D') then
            if (typmo2 .eq. 'PMF') then
                call lctest(comcod, 'MODELISATION', 'PMF', irett)
                if (irett .eq. 0) then
                    texte(1)=notype
                    texte(2)=noma
                    texte(3)=defo
                    call u2mesg('F', 'COMPOR1_52', 3, texte, 0,&
                                0, 0, 0.d0)
                endif
            else
                call lctest(comcod, 'MODELISATION', '1D', irett)
                if (irett .eq. 0) then
                    texte(1)=notype
                    texte(2)=noma
                    texte(3)=defo
                    call u2mesg('F', 'COMPOR1_52', 3, texte, 0,&
                                0, 0, 0.d0)
                endif
            endif
        else
            call lctest(comcod, 'MODELISATION', typmod, irett)
            if (irett .eq. 0) then
                texte(1)=notype
                texte(2)=noma
                texte(3)=defo
                call u2mesg('F', 'COMPOR1_52', 3, texte, 0,&
                            0, 0, 0.d0)
            endif
        endif
!
    endif
    40 end do
!
    call jedema()
!
end subroutine
