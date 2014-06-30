subroutine giecma(nfic, trouve, nbele, nomobj, tymail,&
                  nbno, ecrma, icoma)
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nfic, nbele, nbno, icoma, ibid
    character(len=8) :: tymail, nomobj
    logical(kind=1) :: trouve, ecrma(*)
! ----------------------------------------------------------------------
!     BUT: ECRIRE SUR LE FICHIER DE MAILLAGE ASTER
!          LES MAILLES CORRESPONDANT A L'OBJET GIBI
!     ( SI CES MAILLES SONT EN DOUBLE, ON NE LES REECRIT PAS)
!
!     IN : NFIC : UNITE D'ECRITURE
!          TROUVE : LE GROUP_MA COURANT EST-IL A TRAITE
!            SI OUI : ON ECRIT LA CONNECTIVITE ET ON INCREMENTE ICOMA
!            SI NON : ON INCREMENTE ICOMA MAIS ON N'ECRIT RIEN.
!          NBELE: NOMBRE DE MAILLES DANS L'OBJET.
!          NOMBOJ:NOM DE LA SD CONTENANT LA CONNECTIVITE.
!          TYMAIL:TYPE_MAILLE (GIBI)
!          NBNO : NOMBRE DE NOEUD DE TYMAIL.
!     VAR: ICOMA : COMPTEUR DE MAILLE.
!          (ICOMA EST INCREMENTE DE NBELE A CHAQUE APPEL)
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!-----------------------------------------------------------------------
    integer :: i,  iacorr,   ibvec, icoj
    integer :: icok, ii, itymai, ivect, j, k, l
    integer :: maili, maille, nbelem, nbfois, nbrest, nmtot, numno
!
!-----------------------------------------------------------------------
    parameter (nbelem = 18)
    character(len=7) :: k7nom(8)
    character(len=8) :: k8nom(8), tymagi(nbelem), tymaas(nbelem)
!
!
!     COGIAS EST UN TAMPON POUR ECRIRE LA CONNECTIVITE DES MAILLES
!        DANS L'ORDRE ASTER . (27 EST LE MAX DE NOEUDS POSSIBLE).
    integer :: cogias(27)
    integer, pointer :: indirect(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: numanew(:) => null()
    data tymaas/    'POI1    ','SEG2    ','SEG3    ','TRIA3   ',&
     &     'TRIA6   ','QUAD4   ','QUAD8   ','QUAD9   ','TETRA4  ',&
     &     'TETRA10 ','PENTA6  ','PENTA15 ','HEXA8   ','HEXA20  ',&
     &     'HEXA27  ','PYRAM5  ','PYRAM13 ','????    '/
    data tymagi/    'POI1    ','SEG2    ','SEG3    ','TRI3    ',&
     &     'TRI6    ','QUA4    ','QUA8    ','QUA9    ','TET4    ',&
     &     'TE10    ','PRI6    ','PR15    ','CUB8    ','CU20    ',&
     &     'CU27    ','PYR5    ','PY13    ','????    '/
!
!
    call jemarq()
    if (nbno .gt. 27) then
        call utmess('F', 'PREPOST_54')
    endif
!
    call jeveuo('&&GILIRE'//nomobj//'.CONNEX', 'L', vi=connex)
    call jeveuo('&&GILIRE.NUMANEW', 'E', vi=numanew)
    call jelira('&&GILIRE.NUMANEW', 'LONUTI', nmtot)
    call jeexin('&&GILIRE.INDIRECT', ibid)
    if (ibid .eq. 0) then
        call utmess('F', 'PREPOST_55')
    endif
    call jeveuo('&&GILIRE.INDIRECT', 'L', vi=indirect)
!
    call jeexin('&&GILIRE.VECT', ibvec)
    if (ibvec .eq. 0) then
        call wkvect('&&GILIRE.VECT', 'V V I', nmtot, ivect)
        do 111 i = 1, nmtot
            zi(ivect+i-1)=0
111      continue
    else
        call jeveuo('&&GILIRE.VECT', 'L', ivect)
    endif
!
!
!  -- ON VERIFIE QUE LE GROUPE COURANT EST NOMME OU SOUS GROUPE
!
    if (.not.trouve) then
        icoma = icoma + nbele
        goto 9999
    endif
!
    call jeveuo(jexnom('&&GILIRE.CORR_GIBI_ASTER', tymail), 'L', iacorr)
    itymai = indik8(tymagi(1),tymail,1,nbelem)
    if (itymai .eq. 0) then
        call utmess('F', 'PREPOST_56', sk=tymail)
    endif
!
    write (nfic,*) tymaas(itymai)
!
!     -- BOUCLE SUR LES MAILLES DE L'OBJET SIMPLE:
!     --------------------------------------------
    do 1 i = 1, nbele
!
!
        icoma = icoma + 1
        maille = numanew(icoma)
!
!        -- SI LA MAILLE N'A PAS SON NUMERO INITIAL
!           ET SI ELLE EST DEJA ECRITE ON SORT
        if ((maille.ne.icoma) .and. (ecrma(maille))) goto 1
!
! SI LA MAILLE N A PAS LE NUMERO COURANT ET QU'ELLE
! N' A PAS ETE ECRITE ON ECRIT LE NOEUD COURANT
!
        if ((maille.ne.icoma) .and. (.not.(ecrma(maille)))) then
            if (zi(ivect+maille-1) .eq. 0) zi(ivect+maille-1) = icoma
            do 11 ii = 1, nmtot
                maili = numanew(ii)
                if (maili .eq. maille) then
                    numanew(ii)= zi(ivect+maille-1)
                    ecrma(numanew(ii))=.true.
                endif
11          continue
        endif
!
!
        ecrma(maille)=.true.
!
        call codent(icoma, 'G', k7nom(1))
        k8nom(1) = 'M'//k7nom(1)
!
!        -- REMPLISSAGE DE COGIAS:
        do 10 j = 1, nbno
            numno = connex(nbno* (i-1)+j)
            cogias(j) = indirect(numno)
10      continue
!
        nbfois = nbno/7
        nbrest = nbno - 7*nbfois
        icoj = 0
        icok = 0
!
        do 2 j = 1, nbfois
            do 3 k = 1, 7
                icok = icok + 1
                numno = cogias(zi(iacorr-1+icok))
                call codent(numno, 'G', k7nom(1+k))
                k8nom(1+k) = 'N'//k7nom(1+k)
 3          continue
            write (nfic,1001) (k8nom(l),l=1,8)
            k8nom(1) = ' '
            icoj = icoj + 7
 2      continue
!
        do 4 k = 1, nbrest
            icok = icok + 1
            numno = cogias(zi(iacorr-1+icok))
            call codent(numno, 'G', k7nom(1+k))
            k8nom(1+k) = 'N'//k7nom(1+k)
 4      continue
        write (nfic,1001) (k8nom(l),l=1,nbrest+1)
 1  end do
    write (nfic,*) 'FINSF'
    write (nfic,*) '%'
!
9999  continue
    call jelibe('&&GILIRE.VECT')
!
    1001 format (2x,a8,7(1x,a8),1x)
!
    call jedema()
end subroutine
