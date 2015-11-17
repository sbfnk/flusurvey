DROP SEQUENCE epidb_results_weekly_id_seq;
CREATE SEQUENCE epidb_results_weekly_id_seq START WITH 1 INCREMENT BY 1 NO MAXVALUE NO MINVALUE CACHE 1;
ALTER TABLE epidb_results_weekly ADD COLUMN id integer;
UPDATE epidb_results_weekly SET id = nextval('epidb_results_weekly_id_seq');
ALTER TABLE epidb_results_weekly ALTER COLUMN id SET DEFAULT nextval('epidb_results_weekly_id_seq');
ALTER TABLE epidb_results_weekly ALTER COLUMN id SET NOT NULL;
ALTER TABLE epidb_results_weekly ADD UNIQUE(id);
ALTER TABLE epidb_results_weekly DROP CONSTRAINT epidb_results_weekly_id_key RESTRICT;
ALTER TABLE epidb_results_weekly ADD PRIMARY KEY (id);

