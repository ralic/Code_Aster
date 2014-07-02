subroutine uteref(chanom, typech, tyelas, nomte, nomfpg,&
                  nnos, nno, nbpg, ndim, refcoo,&
                  gscoo, wg, codret)
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     UTILITAIRE - ELEMENT DE REFERENCE
!     --           -          ---
!-----------------------------------------------------------------------
!     ENTREES :
!       CHANOM : NOM ASTER DU CHAMP
!       TYPECH : TYPE DU CHAMP ('ELGA')
!       TYELAS : TYPE D'ELEMENT ASTER
!       NOMTE  : NOM DE L'ELEMENT FINI A EXAMINER
!     SORTIES :
!       NOMFPG : NOM DE LA FAMILLE DES POINTS DE GAUSS
!       NNOS   : NOMBRE DE NOEUDS SOMMETS
!       NNO    : NOMBRE DE NOEUDS TOTAL
!       NBPG   : NOMBRE DE POINTS DE GAUSS
!       NDIM   : DIMENSION DE L'ELEMENT
!       REFCOO : COORDONNEES DES NNO NOEUDS
!       GSCOO  : COORDONNEES DES POINTS DE GAUSS, SI CHAMP ELGA
!       WG     : POIDS DES POINTS DE GAUSS, SI CHAMP ELGA
!       CODRET : CODE DE RETOUR
!                0 : PAS DE PB
!                1 : LE CHAMP N'EST PAS DEFINI SUR CE TYPE D'ELEMENT
!     REMARQUE :
!     ON DOIT RETOURNER LES COORDONNEES SOUS LA FORME :
!     . ELEMENT 1D : X1 X2 ... ... XN
!     . ELEMENT 2D : X1 Y1 X2 Y2 ... ... XN YN
!     . ELEMENT 3D : X1 Y1 Z1 X2 Y2 Z2 ... ... XN YN ZN
!     C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!     ON DOIT RETOURNER LES POIDS SOUS LA FORME :
!     WG1 WG2 ... ... WGN
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/elraca.h"
#include "asterfort/elraga.h"
#include "asterfort/elref2.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsutor.h"
#include "asterfort/utmess.h"
!
    integer :: tyelas
    integer :: nnos, nno, nbpg, ndim
!
    real(kind=8) :: refcoo(*), gscoo(*), wg(*)
!
    character(len=8) :: typech
    character(len=16) :: nomte
    character(len=16) :: nomfpg
    character(len=19) :: chanom
!
    integer :: codret
!
!
! 0.3. ==> VARIABLES LOCALES
!
    integer :: nbfamx
    parameter ( nbfamx = 20 )
!
    integer :: nbpg00(nbfamx), imolo, nec, kfpg, nbfpg, nbfpg2, ifam
    integer :: itype, nb1, nbelr, jmolo, idime, ipg
    integer :: ifm, nivinf, igrel
    integer :: ierd, jliel, nbgrel, iordr
    integer :: iaux, dimtopo
    aster_logical :: ljoint, lpenta
!
    integer :: lgmax
    parameter (lgmax=1000)
!
    real(kind=8) :: gscoo2(3*lgmax)
!
    character(len=4) :: tych
    character(len=8) :: elrefe, elrefb, lielrf(nbfamx), fapg(nbfamx), nomgd, famil
    character(len=8) :: nomtypmail, fapg2(nbfamx)
    character(len=16) :: nomsym, valk(2)
    character(len=19) :: ligrel, resu
!
    real(kind=8) :: vol
    integer, pointer :: celd(:) => null()
    character(len=24), pointer :: celk(:) => null()
!-----------------------------------------------------------------------
!     1- PREALABLES
!     ---------------
    call jemarq()
    codret = 0
    ljoint = .false.
!
    call infniv(ifm, nivinf)
    if (nivinf .gt. 1) then
        call utmess('I', 'UTILITAI5_39')
        write (ifm,10001) tyelas, nomte
        10001 format('ELEMENT FINI NUMERO',i6,', DE NOM : ',a16)
    endif
    ASSERT(typech.eq.'ELGA')
!
!     2- DETERMINATION DE ELREFE :
!     -----------------------------
!
    if (codret .eq. 0) then
!
        call elref2(nomte, nbfamx, lielrf, nbelr)
        ASSERT(nbelr.gt.0)
        elrefe = lielrf(1)
!
!
        call dismoi('TYPE_CHAMP', chanom, 'CHAMP', repk=tych)
        ASSERT(tych.eq.typech)
        call jeveuo(chanom//'.CELK', 'L', vk24=celk)
        call jeveuo(chanom//'.CELD', 'L', vi=celd)
        ligrel = celk(1)(1:19)
        ASSERT(celk(3)(1:19).eq.typech)
        nbgrel = celd(2)
!
    endif
!
!     3- DETERMINATION DU MODE_LOCAL (IMOLO) ASSOCIE AU CHAMP POUR
!        LE TYPE D'ELEMENT TYELAS :
!     -------------------------------------------------------------
!
    if (codret .eq. 0) then
!
        do 31 ,igrel=1,nbgrel
!
        call jeveuo(jexnum(ligrel//'.LIEL', igrel), 'L', jliel)
        call jelira(jexnum(ligrel//'.LIEL', igrel), 'LONMAX', nb1)
        itype = zi(jliel-1+nb1)
!
        if (itype .eq. tyelas) then
            imolo = celd(celd(4+igrel)+2)
            if (imolo .eq. 0) then
                codret = 1
                if (nivinf .gt. 1) then
                    write (ifm,*)&
     &          '==> LE CHAMP N''EST PAS DEFINI SUR CE TYPE D''ELEMENT'
                endif
            endif
            goto 32
        endif
!
        31     end do
!
        ASSERT(.false.)
!
 32     continue
!
    endif
!
!     4- DETERMINATION DE LA FAMILLE DE POINTS DE GAUSS :
!     -------------------------------------------------------------
!
    if (codret .eq. 0) then
!
        call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
        call dismoi('NOM_GD', chanom, 'CHAMP', repk=nomgd)
        call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
        kfpg = zi(jmolo-1+4+nec+1)
        call jenuno(jexnum('&CATA.TM.NOFPG', kfpg), nomfpg)
        ASSERT(elrefe.eq.nomfpg(1:8))
        famil = nomfpg(9:16)
!
    endif
!
!     5- APPEL AUX ROUTINES ELRACA ET ELRAGA DE DESCRIPTION DES ELREFE:
!     ----------------------------------------------------------------
!
    lpenta = .false.
    if (codret .eq. 0) then
!
        call elraca(elrefe, ndim, nno, nnos, nbfpg,&
                    fapg, nbpg00, refcoo, vol)
!
        call dismoi('DIM_TOPO', nomte, 'TYPE_ELEM', repi=dimtopo)
        call dismoi('NOM_TYPMAIL', nomte, 'TYPE_ELEM', repk=nomtypmail)
!       Glute pour les joints
        elrefb = elrefe
        if (dimtopo .ne. ndim) then
            if (nomtypmail .eq. 'HEXA8') then
                elrefb = 'HE8'
            else if (nomtypmail.eq.'QUAD8') then
                elrefb = 'QU8'
            else if (nomtypmail.eq.'PENTA6') then
                lpenta = .true.
                elrefb = 'PE6'
            else if (nomtypmail.eq.'PENTA15') then
                lpenta = .true.
                elrefb = 'P15'
            else if (nomtypmail.eq.'HEXA20') then
                elrefb = 'H20'
            else
                call utmess('F', 'MED2_11')
            endif
            call elraca(elrefb, dimtopo, nno, nnos, nbfpg2,&
                        fapg2, nbpg00, refcoo, vol)
            ljoint = .true.
        endif
!
        ASSERT(nbfpg.le.20)
        ASSERT(nno.le.27)
        ifam = indik8(fapg,famil,1,nbfpg)
        if (ifam .le. 0) then
            resu = chanom(1:8)
            call jeexin(resu//'.DESC', ierd)
            if (ierd .ne. 0) then
                call rsutor(resu, chanom, nomsym, iordr)
            else
                nomsym = chanom
            endif
            valk(1) = nomsym
            valk(2) = famil
            call utmess('F', 'MED2_5', nk=2, valk=valk)
        endif
!
        if (ljoint) then
            call elraga(elrefe, famil, ndim, nbpg, gscoo2,&
                        wg)
            if (lpenta) then
                do ipg = 1, nbpg
                    do idime = 1, dimtopo
                        if (idime .eq. 1) then
                            gscoo((ipg-1)*dimtopo + idime) = 0
                        else
                            gscoo((ipg-1)*dimtopo + idime) = gscoo2((ipg-1)*ndim + idime-1)
                        endif
                    enddo
                enddo
            else
                do ipg = 1, nbpg
                    do idime = 1, dimtopo
                        if (idime .gt. ndim) then
                            gscoo((ipg-1)*dimtopo + idime) = 0
                        else
                            gscoo((ipg-1)*dimtopo + idime) = gscoo2((ipg-1)*ndim + idime)
                        endif
                    enddo
                enddo
            endif
            ndim = dimtopo
        else
            call elraga(elrefe, famil, ndim, nbpg, gscoo,&
                        wg)
        endif
!
        ASSERT(nbpg.le.27)
!
    endif
!
!     6- IMPRESSION EVENTUELLE SUR LE FICHIER DE MESSAGES
!     ----------------------------------------------------------------
!
    if (codret .eq. 0) then
!
        if (nivinf .gt. 1) then
!
            write (ifm,61000) 'FAMILLE DE POINTS DE GAUSS', nomfpg
            write (ifm,61001) 'NOMBRE DE SOMMETS        ', nnos
            write (ifm,61001) 'NOMBRE DE NOEUDS         ', nno
            write (ifm,61001) 'NOMBRE DE POINTS DE GAUSS', nbpg
!
!     6.1. DIMENSION 1
!
            if (ndim .eq. 1) then
!                            123456789012345
                write (ifm,60001) 'NOEUDS         '
                do 6011 , iaux = 1 , nno
                write (ifm,60011) iaux,refcoo(iaux)
6011             continue
                write (ifm,60021)
                write (ifm,60001) 'POINTS DE GAUSS'
                do 6021 , iaux = 1 , nbpg
                write (ifm,60011) iaux,gscoo(iaux)
6021             continue
                write (ifm,60021)
!
!     6.2. DIMENSION 2
!
            else if (ndim.eq.2) then
                write (ifm,60002) 'NOEUDS         '
                do 6012 , iaux = 1 , nno
                write (ifm,60012) iaux, refcoo(ndim*(iaux-1)+1),&
                    refcoo(ndim*(iaux-1)+2)
6012             continue
                write (ifm,60022)
                write (ifm,60002) 'POINTS DE GAUSS'
                do 6022 , iaux = 1 , nbpg
                write (ifm,60012) iaux, gscoo(ndim*(iaux-1)+1),&
                    gscoo(ndim*(iaux-1)+2)
6022             continue
                write (ifm,60022)
!
!     6.3. DIMENSION 3
!
            else
                write (ifm,60003) 'NOEUDS         '
                do 6013 , iaux = 1 , nno
                write (ifm,60013) iaux, refcoo(ndim*(iaux-1)+1),&
                    refcoo(ndim*(iaux-1)+2), refcoo(ndim*(iaux-1)+3)
6013             continue
                write (ifm,60023)
                write (ifm,60003) 'POINTS DE GAUSS'
                do 6023 , iaux = 1 , nbpg
                write (ifm,60013) iaux, gscoo(ndim*(iaux-1)+1),&
                    gscoo(ndim*(iaux-1)+2), gscoo(ndim*(iaux-1)+3)
6023             continue
                write (ifm,60023)
            endif
!
            write (ifm,60004)
            do 6024 , iaux = 1 , nbpg
            write (ifm,60011) iaux, wg(iaux)
6024         continue
            write (ifm,60021)
!
        endif
!
    endif
!
    60001 format(&
     &/,28('*'),&
     &/,'*      COORDONNEES DES     *',&
     &/,'*      ',a15        ,'     *',&
     &/,28('*'),&
     &/,'*  NUMERO  *       X       *',&
     &/,28('*'))
    60002 format(&
     &/,44('*'),&
     &/,'*       COORDONNEES DES ',a15        ,'    *',&
     &/,44('*'),&
     &/,'*  NUMERO  *       X       *       Y       *',&
     &/,44('*'))
    60003 format(&
     &/,60('*'),&
     &/,'*            COORDONNEES DES ',a15         ,&
     &'               *',&
     &/,60('*'),&
     &/,'*  NUMERO  *       X       *       Y       *',&
     &'       Z       *',&
     &/,60('*'))
    60004 format(&
     &/,28('*'),&
     &/,'*      POINTS DE GAUSS     *',&
     &/,'*  NUMERO  *     POIDS     *',&
     &/,28('*'))
    60011 format('* ',i5,'    *',1pg12.5,'    *')
    60012 format('* ',i5,2('    *',1pg12.5),'    *')
    60013 format('* ',i5,3('    *',1pg12.5),'    *')
    60021 format(28('*'))
    60022 format(44('*'))
    60023 format(60('*'))
    61000 format(a,' : ',a)
    61001 format(a,' : ',i4)
!
    call jedema()
end subroutine
