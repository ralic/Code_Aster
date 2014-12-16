subroutine apbloc(kptsc)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
    integer :: kptsc
! person_in_charge: natacha.bereux at edf.fr
#include "asterf_types.h"
#include "asterf_petsc.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/wkvect.h"
#include "asterfort/jexnum.h"
#include "asterfort/jedetr.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!----------------------------------------------------------------
!
!  * Determination du nombre de ddls par noeud (le "blocksize" de PETSc)
!  * Eventuel ajout de ddls fictifs pour les preconditionneurs multigrille
!    dans le cas ou l'utilisateur a demande ELIM_LAGR='OUI'
!
!  Le "blocksize" n'est utilisable qu'avec certains preconditionneurs
!  multigrille : BOOMER, ML, GAMG
!
!  in  : kptsc : indice dans le common
!  out
!    tblocs(kptsc)   : "blocksize" a utiliser dans PETSc
!    fictifs(kptsc)  : 0/1  (On a ajoute des ddls fictifs ou non)
!    si fictifs(kptsc) == 1 :
!       new_ieqs(kptsc)(ieq1) -> ieq2
!       old_ieqs(kptsc)(ieq2) -> ieq1
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
!----------------------------------------------------------------
!   -- variables locales :
    character(len=19) :: matass, solveu
    character(len=14) :: nonu
    character(len=8) :: nomgd,noma,exilag
    integer :: tbloc, tbloc2
    character(len=24) :: precon
    aster_logical :: leliml, ndiff, leliml2
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: smdi(:) => null()
    integer(kind=4), pointer :: smhc(:) => null()

    integer(kind=4), pointer :: nbterm(:) => null()
    integer(kind=4), pointer :: pcumu(:) => null()
    integer(kind=4), pointer :: posterm(:) => null()
    integer(kind=4), pointer :: new_ieq(:) => null()
    integer(kind=4), pointer :: old_ieq(:) => null()
    character(len=24), pointer :: refa(:) => null()

    integer:: k,jcol,ilig,neq,nzdeb,nzfin,kec,nbec,nbnoma,nbddl
    integer:: jprno,n1,ino,ino_model,ecmax(10),ec1,ec2,nnz,pos1,pos2,nbloc
    integer:: kbloc,i,ilig1,ilig2,k2,pos1_0,ieq,ieq1,ieq2
    integer:: fictif,jnueq,kcmp,neq2,nbnomo,nbddlt,ico,vali(4)
    aster_logical :: dbg=.false.
!
!----------------------------------------------------------------
    call jemarq()
    tbloc=1
    fictif=0
    matass = nomats(kptsc)
    if (dbg) write(6,*) 'apbloc matass=',matass
    solveu = nosols(kptsc)
    nonu = nonus(kptsc)


!   -- Quel preconditionneur ?
!      tbloc > 1 n'est utile que pour les precondionneurs  BOOMER, ML et GAMG
!   ---------------------------------------------------------------------------
    call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
    precon = slvk(2)
    if (dbg) write(6,*) 'apbloc precon=',precon
    if ((precon.ne.'ML') .and. (precon.ne.'BOOMER') .and. (precon.ne.'GAMG')) then
        if (dbg) write(6,*) "apbloc tbloc impose a 1 car PRE_COND ne l'utilise pas."
        tbloc = 1
        goto 999
    endif


!   -- La matrice/solveur est-elle ELIM_LAGR='OUI' ?  A-t-elle des ddls de Lagrange ?
!   -----------------------------------------------------------------------------------
    leliml = slvk(13)(1:3).eq.'OUI'
    call dismoi('EXIS_LAGR', matass, 'MATR_ASSE', repk=exilag)


!   --  Si ELIM_LAGR='OUI', la matrice ne sera pas utilisee telle quelle pour la resolution Petsc.
!       On va d'abord la reduire.
!       => tbloc > 1 n'a pas d'interet.
!   ----------------------------------------------------------------------------------------------
    if (leliml) then
        if (dbg) write(6,*) 'apbloc tbloc impose a 1 car ELIM_LAGR=OUI'
        tbloc = 1
        goto 999
    endif


!   --  Si ELIM_LAGR='NON', on regarde si la matrice n'est pas la matrice reduite
!       associee a une matrice qui a utilise ELIM_LAGR='OUI'
!       Si oui, on autorisera l'ajout de ddls fictifs.  (=> leliml2)
!   ----------------------------------------------------------------------------------------------
    leliml2=.false.
    if (.not.leliml) then
        call jeveuo(matass//'.REFA', 'L', vk24=refa)
        if (refa(20).ne.' ') leliml2=.true.
    endif
    if (dbg) write(6,*) 'apbloc leliml2=',leliml2


!   -- S'il y a des ddls de Lagrange, c'est cuit !
!   -------------------------------------------------------------------------------
    if (exilag.eq.'OUI')  call utmess('F', 'PETSC_18')


!   -- 1. On ne peut utiliser tbloc > 1 que si TOUS les noeuds du maillage portent les memes ddls
!         (memes entiers codes)
!         On va calculer :
!           * tbloc (si possible, sinon : erreur <F>)
!           * fictif=0/1
!              0 : Tous les noeuds ont exactement le meme nombre de ddls (tbloc)
!                  (sans ajouter de ddls fictifs)
!              1 : Tous les noeuds auront le meme nombre de ddls si on ajoute des ddls fictifs
!   -------------------------------------------------------------------------------------------
    call dismoi('NOM_MAILLA', matass, 'MATR_ASSE', repk=noma)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoma)
    call dismoi('NOM_GD', nonu, 'NUME_DDL', repk=nomgd)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
    ASSERT(nbec.le.10)
    call jeveuo(jexnum(nonu//'.NUME.PRNO', 1), 'L', jprno)
    call jelira(jexnum(nonu//'.NUME.PRNO', 1), 'LONMAX', n1)
    ASSERT(n1.eq.nbnoma*(nbec+2))
    if (dbg) write(6,*) 'apbloc nbec,nbnoma=',nbec,nbnoma

!   -- on interdit le cas NUEQ(ieq) /= ieq :
    call jeveuo(nonu//'.NUME.NUEQ','L', jnueq)
    call jelira(nonu//'.NUME.NUEQ', 'LONMAX', neq)
    do ieq=1,neq
        ASSERT(zi(jnueq-1+ieq).eq.ieq)
    enddo

!   -- calcul de fictif :
!   ---------------------
    ecmax(1:nbec)=0
    do ino=1,nbnoma
       nbddl=zi(jprno-1+(nbec+2)*(ino-1)+2)
       if (nbddl.eq.0) cycle
       do kec=1,nbec
           ec1=zi(jprno-1+(nbec+2)*(ino-1)+2+kec)
           ecmax(kec)=ior(ecmax(kec),ec1)
       enddo
    enddo
    fictif=1
    if (dbg) write(6,*) 'apbloc ecmax=',ecmax(1:nbec)

!   -- calcul de tbloc :
!   --------------------
    tbloc=0
    do kec=1,nbec
        do kcmp=1,30
            if (iand(ecmax(kec),2**kcmp).eq.2**kcmp) tbloc=tbloc+1
        enddo
    enddo
    if (dbg) write(6,*) 'apbloc tbloc =',tbloc


!   -- les entiers codes sont-ils les memes sur TOUS les noeuds ?
!   -------------------------------------------------------------
    ino_model=0
    nbnomo=0
    nbddlt=0
    ndiff=.false.
    do ino=1,nbnoma
       nbddl=zi(jprno-1+(nbec+2)*(ino-1)+2)
       if (nbddl.eq.0) cycle

       nbnomo=nbnomo+1
       nbddlt=nbddlt+nbddl
       if (ino_model.eq.0) then
           ino_model=ino
           tbloc2=nbddl
           if (dbg) write(6,*) 'apbloc ino_model,tbloc=',ino_model,tbloc2
       endif
       if (.not.ndiff) then
           do kec=1,nbec
               ec1=zi(jprno-1+(nbec+2)*(ino-1)+2+kec)
               ec2=zi(jprno-1+(nbec+2)*(ino_model-1)+2+kec)
               if (ec1 .ne. ec2) ndiff=.true.
           enddo
       endif
    enddo

    if (.not.ndiff) then
!       -- Tous les noeuds portent les memes ddls, on peut remettre fictif a 0 :
!       ------------------------------------------------------------------------
        ASSERT(tbloc.eq.tbloc2)
        fictif=0

    else
        if (leliml2) then
!           -- On ajoute des ddls fictifs :
!           --------------------------------
            n1=nbnomo*tbloc - nbddlt
            ASSERT(n1.gt.0)
            if (dbg) write(6,*) 'apbloc nb_ddl originaux =', nbddlt
            if (dbg) write(6,*) 'apbloc nb_ddl fictifs   =', n1
            vali(1)=n1
            vali(2)=int(100*float(n1)/nbddlt)
            call utmess('I', 'PETSC_21', ni=2, vali=vali)
!           fictif=0; tbloc=1
        else
!           -- On interdit l'ajout de ddls fictifs :
!              Pour passer outre, changer l'erreur <F> en <A>larme.
!           -------------------------------------------------------
            call utmess('F', 'PETSC_22')
        endif
    endif
    if (dbg) write(6,*) 'apbloc fictif,tbloc =',fictif,tbloc



    if (tbloc.eq.1) goto 999


!   -- 2. Si fictif = 1, On calcule :
!         * le vecteur new_ieq(ieq1)=ieq2 (>=ieq1) avec :
!           ieq1 : numero d'equation avant l'ajout des ddls fictifs
!           ieq2 : numero d'equation apres l'ajout des ddls fictifs
!         * le vecteur old_ieq(ieq2)=ieq1 (<=ieq2) avec :
!           ieq1>0 : numero d'equation "avant" correspondant a ieq2
!           ieq1=0 : l'equation "apres" ieq2 correspond a un ddl fictif
!   ------------------------------------------------------------------------
    if (fictif.eq.1) then
        allocate(new_ieq(neq))
        ieq1=0
        ieq2=0
        do ino=1,nbnoma
           ico=0
           do kec=1,nbec
               ec1=zi(jprno-1+(nbec+2)*(ino-1)+2+kec)
               if (ec1.gt.0) then
                   do kcmp=1,30
                       if (iand(ecmax(kec),2**kcmp).eq.2**kcmp) then
                           ieq2=ieq2+1
                           if (iand(ec1,2**kcmp).eq.2**kcmp) then
                               ieq1=ieq1+1
!                              -- On verifie que les ddls sont numerotes dans l'ordre des
!                                 noeuds du maillage.
!                                 Avec PETSc, on n'a pas le droit d'utiliser le mot cle RENUM
                               ASSERT(ieq1.eq.zi(jprno-1+(nbec+2)*(ino-1)+1)+ico)
                               ico=ico+1

                               new_ieq(ieq1)=ieq2
                           else
                               if (dbg) write(6,*) 'apbloc ddl fictif ino,kcmp=',ino,kcmp
                           endif
                       endif
                   enddo
               endif
           enddo
        enddo
        ASSERT(ieq1.eq.neq)
        neq2=ieq2
        if (dbg) write(6,*) 'apbloc neq,neq2=',neq,neq2

        ASSERT(mod(neq2,tbloc).eq.0)
        if (dbg) write(6,*) 'apbloc nb_ddls fictifs=',neq2-neq

        allocate(old_ieq(neq2))
        old_ieq=0
        do ieq1=1,neq
            ieq2=new_ieq(ieq1)
            old_ieq(ieq2)=ieq1
        enddo

        if (.false.) then
            write(6,*) 'apbloc new_ieq=',new_ieq(:)
            write(6,*) 'apbloc old_ieq=',old_ieq(:)
        endif
    endif



999 continue

!   -- sauvegarde des informations calculees dans le common :
!   ---------------------------------------------------------
    ASSERT(tblocs(kptsc).eq.-1)
    ASSERT(fictifs(kptsc).eq.-1)
    tblocs(kptsc)=tbloc
    fictifs(kptsc)=fictif
    if (fictif.eq.1) then
        ASSERT(.not.associated(new_ieqs(kptsc)%pi4))
        ASSERT(.not.associated(old_ieqs(kptsc)%pi4))
        new_ieqs(kptsc)%pi4 => new_ieq
        old_ieqs(kptsc)%pi4 => old_ieq
    endif

    call jedema()
    if (dbg) write(6,*) 'apbloc tbloc,fictif=',tbloc,fictif

#endif

end subroutine
