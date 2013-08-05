subroutine dismms(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(MATR_ASSE) (MARCHE AUSSI PARFOIS SUR MATR_ASSE_GENE)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismgd.h"
#include "asterfort/dismme.h"
#include "asterfort/dismnu.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE CONCEPT MATR_ASSE  (K19)
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=32) :: repk
    character(len=24) :: p1, p2, k24
    character(len=19) :: nomob, solveu, prno
    integer :: jrefa, jslvk, jprno, jdeeq
    character(len=8) :: kbid, nomgd
    character(len=7) :: typmat
!-----------------------------------------------------------------------
    integer :: i, ibid, ier, nec, ieq, neq
    integer :: ialime, nblime, nbddl, nbddlc, numno
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    call jeveuo(nomob//'.REFA', 'L', jrefa)
!
!
    if (questi(1:9) .eq. 'NUM_GD_SI') then
        call dismnu(questi, zk24(jrefa-1+2)(1:14), repi, repk, ierd)
    else if (questi(1:9).eq.'NOM_GD_SI') then
        call dismnu('NOM_GD', zk24(jrefa-1+2)(1:14), repi, repk, ierd)
!
    else if (questi.eq.'TYPE_MATRICE') then
        typmat=zk24(jrefa-1+9)
        if (typmat .eq. 'MS') then
            repk='SYMETRI'
        else if (typmat.eq.'MR') then
            repk='NON_SYM'
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'NB_EQUA') then
        call dismnu(questi, zk24(jrefa-1+2)(1:14), repi, repk, ierd)
!
    else if (questi.eq.'NOM_MODELE') then
        call dismnu(questi, zk24(jrefa-1+2)(1:14), repi, repk, ierd)
!
    else if (questi.eq.'NOM_MAILLA') then
        repk= zk24(jrefa-1+1)(1:8)
!
    else if (questi.eq.'NOM_NUME_DDL') then
        repk= zk24(jrefa-1+2)(1:14)
!
    else if (questi.eq.'EXIS_LAGR') then
        call jeexin(nomob//'.CONL', ier)
        if (ier .eq. 0) then
            repk = 'NON'
        else
            repk = 'OUI'
        endif
!
    else if (questi.eq.'NB_DDL_NOEUD') then
        prno = zk24(jrefa-1+2)(1:14)//'.NUME'
        call jeveuo(jexnum(prno//'.PRNO', 1), 'L', jprno)
        call jeveuo(prno//'.DEEQ', 'L', jdeeq)
!
        call dismnu('NOM_GD', zk24(jrefa-1+2)(1:14), ibid, nomgd, ierd)
        if (ierd .ne. 0) goto 999
        call dismgd('NB_EC', nomgd, nec, kbid, ierd)
        if (ierd .ne. 0) goto 999
        call dismnu('NB_EQUA', zk24(jrefa-1+2)(1:14), neq, kbid, ierd)
        if (ierd .ne. 0) goto 999
!
        nbddl = zi(jprno-1+2)
        do 100 ieq = 2, neq
            numno = zi(jdeeq-1+(ieq -1)* 2 +1)
            nbddlc = zi(jprno-1+(numno-1)*(2+nec)+2)
            if (nbddlc .ne. nbddl) then
                repi = -1
                goto 200
            endif
100      continue
        repi = nbddl
200      continue
!
    else if (questi.eq.'SOLVEUR') then
        if (zk24(jrefa-1+7) .ne. ' ') then
            repk=zk24(jrefa-1+7)
        else
            call dismnu(questi, zk24(jrefa-1+2)(1:14), repi, repk, ierd)
        endif
!
    else if (questi.eq.'METH_RESO'.or.questi.eq.'RENUM_RESO') then
        if (zk24(jrefa-1+7) .ne. ' ') then
            solveu=zk24(jrefa-1+7)(1:19)
            call jeveuo(solveu//'.SLVK', 'L', jslvk)
            if (questi .eq. 'METH_RESO') then
                repk=zk24(jslvk-1+1)
            else
                repk=zk24(jslvk-1+4)
            endif
        else
            call dismnu(questi, zk24(jrefa-1+2)(1:14), repi, repk, ierd)
        endif
!
    else if (questi.eq.'PROF_CHNO') then
        repk= zk24(jrefa-1+2)(1:14)//'.NUME'
!
    else if (questi.eq.'NUME_EQUA') then
        repk= zk24(jrefa-1+2)(1:14)//'.NUME'
!
    else if (questi.eq.'PHENOMENE') then
        call dismnu(questi, zk24(jrefa-1+2)(1:14), repi, repk, ierd)
!
    else if (questi.eq.'SUR_OPTION') then
        repk= zk24(jrefa-1+4)(1:16)
!
    else if (questi.eq. 'MPI_COMPLET') then
        k24 = zk24(jrefa-1+11)
        ASSERT((k24.eq.'MPI_COMPLET') .or. ( k24.eq.'MPI_INCOMPLET') .or. (k24.eq.'MATR_DISTR'))
        if (k24 .eq. 'MPI_COMPLET') then
            repk='OUI'
        else
            repk='NON'
        endif
!
    else if (questi.eq. 'MATR_DISTR') then
        k24 = zk24(jrefa-1+11)
        ASSERT((k24.eq.'MPI_COMPLET') .or. ( k24.eq.'MPI_INCOMPLET') .or. (k24.eq.'MATR_DISTR'))
        if (k24 .eq. 'MATR_DISTR') then
            repk='OUI'
        else
            repk='NON'
        endif
!
        else if((questi.eq.'CHAM_MATER').or. (questi.eq.'CARA_ELEM'))&
    then
        call jeveuo(nomob//'.LIME', 'L', ialime)
        call jelira(nomob//'.LIME', 'LONMAX', nblime, kbid)
        p1=' '
        p2=' '
        ier=0
        do 1, i=1,nblime
        if (zk24(ialime-1+i) .eq. ' ') goto 1
        call dismme(questi, zk24(ialime-1+i)(1:19), ibid, p1, ierd)
        if (p1 .ne. ' ') then
            if (p2 .eq. ' ') then
                p2=p1
            else
                if (p1 .ne. p2) ier=1
            endif
        endif
 1      continue
        if (ier .eq. 0) then
            repk=p2
        else
            repk=' '
            ierd=1
        endif
    else
        ierd=1
    endif
!
    repkz = repk
999  continue
    call jedema()
end subroutine
